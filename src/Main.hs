import Hash
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let argc = length args
  if argc > 0 then
    runScript $ head args
  else
    runInteractive
