module Main where

import qualified Data.ByteString as BS

import System.Environment (getArgs)

import Text.PrettyPrint.Leijen.Text ()

import Prolog.Programming.Data (Code (..), Config (..))
import Prolog.Programming.Task

main :: IO ()
main = do
  args <- getArgs
  case args of
    [task, solution] -> do
      config <- Config <$> readFile task
      code <- Code <$> readFile solution
      runMain config code
    _ -> putStrLn "usage test-task-prolog <task> <solution>"

runMain :: Config -> Code -> IO ()
runMain config code = do
  verifyConfig config
  checkSyntax (fail . show) print writeTreeToDisk config code
  checkSemantics (fail . show) print config code

writeTreeToDisk :: BS.ByteString -> IO ()
writeTreeToDisk g = BS.writeFile "tree.svg" g >> putStrLn "wrote tree to file://tree.svg"
