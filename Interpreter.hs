--
-- Interpreter
-- 
module Interpreter where

import qualified Parser

run :: Parser.Grid -> IO()
run (Parser.Grid grid width height) = putStrLn "yo"
