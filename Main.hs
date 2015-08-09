--
-- Main File
--
module Main where

import System.Environment
import qualified Parser
import qualified Syntax
import qualified Interpreter

main :: IO()
main = do
  args <- getArgs
  case args of
    (h:[]) -> do  {file_stream <- readFile h;
                    if Syntax.checker file_stream then
                      Interpreter.run $ Parser.parse file_stream
                    else 
                      putStrLn "Error syntax"}
    _      -> putStrLn "Error arguments"
