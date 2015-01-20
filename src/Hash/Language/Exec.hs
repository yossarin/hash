module Hash.Language.Exec where

import qualified Data.Map as M
import Hash.Language.Expressions
import Data.Maybe (fromMaybe, isJust, fromJust)

-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output :: String
                               , wd :: FilePath
                               , vartable :: VarTable
                               } deriving Show
-- Evaluates the expression
evalExpr :: VarTable -> Expr -> String
evalExpr vt (Str s) = s
evalExpr vt (Var s) = fromMaybe (error $ "The variable " ++ s ++ " is not initialised.") $ M.lookup s vt

-- Evaluates the command
-- (Assign)
evalCmd :: CommandTable -> ScriptState -> Cmd -> IO ScriptState
evalCmd _  ss (Assign (Var a) b) = do
  let vt   = vartable ss
  let eval = evalExpr vt
  return $ ss { vartable = M.insert a (eval b) vt }
evalCmd _  ss (Assign a b) = error $ "Error: Assign " ++ (eval a) ++ " " ++ (eval b)
  where eval = evalExpr (vartable ss)

-- (Cmd) TODO : redirecting in/out
evalCmd ct ss (Cmd cmdName args inDir outDir isAppend) = do
  let eval = evalExpr (vartable ss)
  let name = eval cmdName
  let cmd  = fromMaybe (error $ "Unknown command '" ++ name ++ "'.") (M.lookup name ct)
  -- redirecting input
  ss' <- if isJust inDir then do
              file <- readFile . eval $ fromJust inDir
              return $ ss { output = file} 
            else return $ ss { output = ""}
  -- redirecting output
  if isJust outDir then do
    let file = eval $ fromJust outDir
    state <- cmd (map eval args) ss'
    let out = output state
    if isAppend then do
      appendFile file out
    else writeFile file out
    return state
  else do
    printSS =<< cmd (map eval args) ss'
  

-- Evalate conditional
evalComp :: ScriptState -> Comp -> Bool
evalComp ss c = let vs = evalExpr (vartable ss) in 
  case c of
    CEQ e1 e2 -> (read $ vs e1 :: Double) == (read $ vs e2 :: Double)
    CNE e1 e2 -> (read $ vs e1 :: Double) /= (read $ vs e2 :: Double)
    CLT e1 e2 -> (read $ vs e1 :: Double) <  (read $ vs e2 :: Double)
    CLE e1 e2 -> (read $ vs e1 :: Double) <= (read $ vs e2 :: Double)
    CGT e1 e2 -> (read $ vs e1 :: Double) >  (read $ vs e2 :: Double)
    CGE e1 e2 -> (read $ vs e1 :: Double) >= (read $ vs e2 :: Double)

-- Evaluate the logical expression
evalPred :: ScriptState -> Pred -> Bool
evalPred ss p = case p of
  Parens p  -> evalPred ss p
  Not p     -> not $ evalPred ss p
  And p1 p2 -> evalPred ss p1 && evalPred ss p2
  Or p1 p2  -> evalPred ss p1 || evalPred ss p2
  Pred c    -> evalComp ss c

-- Prints the output of given ScriptState, return ScriptState with blank output
printSS :: ScriptState -> IO ScriptState
printSS ss = putStrLn (output ss) >> return ss

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command’s execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram _  (Left fp) []     = return ScriptState { wd = fp, output = "", vartable = M.empty }
runHashProgram ct (Left fp) (x:xs) = do
  ss' <- runTopLevel ct (ScriptState { wd = fp, output = "", vartable = M.empty }) x
  runHashProgram ct (Right ss') xs
runHashProgram _  (Right ss) []     = return ss
runHashProgram ct (Right ss) (x:xs) = do
  ss' <- runTopLevel ct ss x
  runHashProgram ct (Right ss') xs

-- Calculates the result of a top-level command execution
-- (Cmd)
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel ct ss (TLCmd cmd) = do
  let eval = evalExpr (vartable ss)
  evalCmd ct ss cmd
-- (Conditional)
runTopLevel ct ss (TLCnd (If cond xs)) = do
  let cmds = if evalPred ss cond then xs else []
  runHashProgram ct (Right ss) (map TLCmd cmds)
runTopLevel ct ss (TLCnd (IfElse cond xs1 xs2)) = do
  let cmds = if evalPred ss cond then xs1 else xs2
  runHashProgram ct (Right ss) (map TLCmd cmds)
