--
-- Interpreter
-- 
module Interpreter where

import qualified Parser
import Data.Vector

--        grid         sens         line  column
step :: Parser.Grid -> (Int,Int) -> Int -> Int -> IO()
step (Parser.Grid grid w h) (dx,dy) line column = 
  case ((grid ! line)! column) of
    '>' -> step (Parser.Grid grid w h) (1,0) line (column+1)
    '<' -> step (Parser.Grid grid w h) (-1,0) line (column-1)
    ' ' -> step (Parser.Grid grid w h) (dx,dy) (line+dy) (column+dx)
    'v' -> step (Parser.Grid grid w h) (0,1) (line+1) column
    '^' -> step (Parser.Grid grid w h) (0,-1) (line-1) column
    '@' -> putStrLn "--exit success--"
    _ -> putStrLn "error, instruction not implemented"

run :: Parser.Grid -> IO()
run grid = 
  step grid (1,0) 0 0
