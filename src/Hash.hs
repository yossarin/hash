module Hash
  ( runScript
  , runInteractive
  ) where

import Hash.Language.Exec
import Hash.Language.Commands
import Hash.Parsing.HashParser (parseToTLExpr) 
import System.Directory (getCurrentDirectory)

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
runInteractive = putStrLn "TODO: runInteractive"

