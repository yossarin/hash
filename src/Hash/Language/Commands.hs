module Hash.Language.Commands where

import qualified Data.Map as M
import Hash.Language.Exec
import System.Directory

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands = M.fromList [ ("pwd",    pwd)
                      , ("mv",     mv )
                      , ("cp",     cp )
                      , ("rm",     rm )
                      , ("create", create)
                      ]

pwd :: Command
pwd _ ss = do
  currentDir <- getCurrentDirectory
  return $ ss { output = currentDir}

{-
   Basic file manipulation commands mv, cp, rm, create.
   They support both absolute and relative paths.
-}

-- Appends filename to directory
apf :: FilePath -> FilePath -> FilePath
apf dir file | last dir == '/' = dir ++ file
                    | otherwise       = dir ++ "/" ++ file

-- Gets filename from FilePath
getFilename :: FilePath -> FilePath
getFilename = reverse . takeWhile (/= '/') . reverse

mv :: Command
mv (src:dest:_) ss = do
  srcEx <- doesFileExist src
  if srcEx then do
    destDir <- doesDirectoryExist dest
    if destDir then do
      copyFile src (apf dest (getFilename src))
      removeFile src
      return $ ss { output = "" }
    else do
      copyFile src dest
      removeFile src
      return $ ss { output = "" }
  else return $ ss { output = ("Error: file '" ++ src ++ "' does not exist :(") }

cp :: Command
cp (src:dest:_) ss = do
  srcEx <- doesFileExist src
  if srcEx then do
    destDir <- doesDirectoryExist dest
    if destDir then do
      copyFile src (apf dest (getFilename src))
      return $ ss { output = "" }
    else do
      copyFile src dest
      return $ ss { output = "" }
  else return $ ss { output = ("Error: file '" ++ src ++ "' does not exist :(") }

rm :: Command
rm []       ss = return ss
rm (f:args) ss = do
  dfe <- doesFileExist f
  let o = output ss
  if dfe then do
    removeFile f
    rm args $ ss { output = o ++ "\nRemoved file '" ++ f ++ "'."}
  else return $ ss { output = o ++ "\nError: '" ++ f ++ "' : no such file :("}

create :: Command
create []     ss = return ss
create (f:fs) ss = do
  writeFile f ""
  let o = output ss
  create fs $ ss { output = o ++ "\nCreated file '" ++ f ++ "'."}
