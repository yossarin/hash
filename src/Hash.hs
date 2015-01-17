module Hash
  ( runScript
  , runInteractive
  ) where

import Control.Applicative ((<$>))
import Hash.Language.Expressions
import Hash.Parsing.HashParser
-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript fp = putStrLn $ "File path: " ++ fp

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = putStrLn "TODO: runInteractive"

