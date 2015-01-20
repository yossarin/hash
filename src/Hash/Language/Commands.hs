module Hash.Language.Commands where

import qualified Data.Map as M
import Hash.Language.Exec
import System.Directory

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands = M.fromList [("pwd", pwd)]

pwd :: Command
pwd _ ss = do
  currentDir <- getCurrentDirectory
  return $ ss { output = currentDir} 
