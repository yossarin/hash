module Hash.Language.Commands where

import qualified Data.Map as M
import Hash.Language.Exec
import System.Directory
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.ByteString.Char8 (pack)
import Hexdump (prettyHex)

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands = M.fromList [ ("pwd",    pwd)
                      , ("mv",     mv )
                      , ("cp",     cp )
                      , ("rm",     rm )
                      , ("create", create)
                      , ("ls",     ls)
                      , ("cd",     cd)
                      , ("mkdir",  mkdir)
                      , ("rmdir",  rmdir)
                      , ("cat",    cat)
                      , ("echo",   echo)
                      , ("hexdump", hexdump)
                      ]

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

{-
  Basic directory manipulation commands:

  mv    (move),
  cpdir (copy),
  mkdir (make directory),
  rmdir (remove directory)

  They support both absolute and relative paths.
-}

mkdir :: Command
mkdir (dir:[]) ss = createDirectory dir >> (return $ ss { output = o ++ "\nmkdir: Created directory '" ++ dir ++ "' :)" })
  where o = output ss
mkdir (dir:dirs) ss = do
  createDirectory dir
  mkdir dirs (ss { output = o ++ "\nmkdir: Created directory '" ++ dir ++ "' :)" })
  where o = output ss
mkdir _       ss = return $ ss { output = "mkdir: no arguments :(" }

rmdir :: Command
rmdir (dir:[]) ss = removeDirectory dir >> (return $ ss { output = "rmdir: removed empty directory '" ++ dir ++ "' :)" })
rmdir _        ss = return $ ss { output = "rmdir: no arguments :(" }

{-
  Basic file system navigation commands:

    ls  (list directory contents),
    pwd (print working/current directory),
    cd  (change directory)

  These commands support both absolute and relative paths
-}

ls :: Command
ls []      ss = do
  content <- getDirectoryContents "."
  return $ ss { output = (intercalate " " content) }

ls (dir:_) ss = do
  content <- getDirectoryContents dir
  return $ ss { output = (intercalate " " content) }

pwd :: Command
pwd _ ss = do
  currentDir <- getCurrentDirectory
  return $ ss { output = currentDir}

cd :: Command
cd []       ss = do
  home <- getHomeDirectory
  setCurrentDirectory home >> (return $ ss { wd = home })
cd (dir:[]) ss = setCurrentDirectory dir >> (return $ ss { wd = dir })

{-
  A basic cat command that dumps the contents of a variable number of files.
-}
cat :: Command
cat []     ss = return ss
cat (x:xs) ss = do
  file <- readFile x
  let o = output ss
  cat xs $ ss { output = o ++ file}

{-
  A basic echo command that repeats user input to the screen, replacing any
  instances of variables with their values.
-}
echo :: Command
echo args ss = return $ ss { output = interpolate vt (head args) ""}
  where vt = vartable ss

-- Interpolation of variables in strings
interpolate :: VarTable -> String -> String -> String
interpolate vt [] acc = acc
interpolate vt s  acc = interpolate vt rest (acc ++ pre ++ val)
  where pre  = takeWhile (/= '$') s
        post = dropWhile (/= '$') s
        var  = if length post > 0 then head . words $ tail post else ""
        rest = drop (length var + 1) post
        val  = lookupVar vt var

-- Returns value of a variable or empty string
lookupVar :: VarTable -> String -> String
lookupVar vt v = fromMaybe "" $ M.lookup v vt

{-
  A basic grep command that matches lines containing a given string literal.
  It supports the -v, -i, -o, -n and -c flags.
  TODO
-}

{-
  hexdump, a command that, given a file, prints out its byte contents
  as hexadecimal numbers.
-}

hexdump :: Command
hexdump args ss = do
  let out = output ss
  
  if out /= "" then do  
    return $ ss { output = prettyHex $ pack out }
  else do
    file <- readFile $ head args
    return $ ss { output = prettyHex $ pack file }
