--
-- Main File
--
module Main where

import System.Environment
import qualified Parser
import qualified Syntax

main :: IO()
main = do
  args <- getArgs
  case args of
    (h:[]) -> do  {file_stream <- readFile h;
                  print $ Syntax.checker file_stream}
    _      -> putStrLn "Error"
