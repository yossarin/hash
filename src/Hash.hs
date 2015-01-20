module Hash
  ( runScript
  , runInteractive
  ) where

import System.IO (hFlush, stdout)
import Hash.Language.Exec
import Hash.Language.Commands
import Hash.Parsing.HashParser (parseToTLExpr) 
import System.Directory (getCurrentDirectory)
import qualified Data.Map as M
import Control.Exception
import Data.Char (isSpace)

-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript fp = do
  file <- readFile fp
  let parsedFile = parseToTLExpr file
  wd <- getCurrentDirectory
  runHashProgram commands (Left wd) parsedFile
  return ()

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
  currentDir <- getCurrentDirectory
  repl (ScriptState {output = "", wd = currentDir, vartable = M.empty}) "hash-0.1> "

-- flushStr prints out a string and immediately flushes the stream
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Prints out a prompt and reads in a line of input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Trim spaces
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Read Eval Print Loop
repl :: ScriptState -> String -> IO ()
repl ss prompt = do
  input <- readPrompt prompt
  let stripped = trim input
  let line = if length stripped > 0 then (if last stripped /= ';' then stripped ++ ";" else stripped) else input

  case line of
    ":q" -> return ()
    _    -> do
     ss' <- catch (runHashProgram commands (Right ss) (parseToTLExpr line)) $
              \e -> do
                putStrLn $ " *** Exception: " ++ show (e :: SomeException)
                return ss
     repl ss' prompt
