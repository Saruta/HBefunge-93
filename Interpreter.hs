--
-- Interpreter
-- 
module Interpreter where

import qualified Parser
import Stack
import Data.Vector
import qualified Data.List as Lst

controlFlow :: Parser.Grid -> (Stack Int)-> (Int,Int) -> Int -> Int 
  -> IO()
controlFlow (Parser.Grid grid w h) s (dx,dy) line column = 
  case ((grid ! line)! column) of
    '>' -> step (Parser.Grid grid w h) s (1,0) line (column+1)
    '<' -> step (Parser.Grid grid w h) s (-1,0) line (column-1)
    ' ' -> step (Parser.Grid grid w h) s (dx,dy) (line+dy) (column+dx)
    'v' -> step (Parser.Grid grid w h) s (0,1) (line+1) column
    '^' -> step (Parser.Grid grid w h) s (0,-1) (line-1) column
    '@' -> putStrLn "--exit success--"
    

-- arithmetic
arithm :: Parser.Grid -> (Stack Int) -> (Int,Int) -> Int -> Int -> IO()
arithm (Parser.Grid grid w h)  s  (dx,dy) line column =
  case ((grid ! line)! column) of
    '+' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (x+y) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
    '-' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (x+y) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
    '*' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (x+y) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
    '/' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (x+y) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
    '%' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (x+y) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
--
--
user :: Parser.Grid -> (Stack Int) -> (Int,Int) -> Int -> Int -> IO()
user (Parser.Grid grid w h) s (dx,dy) line column = 
  case ((grid ! line)! column) of
    '.' -> let (x,s') = pop s in
      do {print x;
      step (Parser.Grid grid w h) s' (dx,dy) (line+dy) (column+dx)}
-- misc

--        grid                    sens         line  column
step :: Parser.Grid -> (Stack Int) -> (Int,Int) -> Int -> Int -> IO()
step (Parser.Grid grid w h) s (dx,dy) line column = 
  case ((grid ! line)! column) of
    c | Lst.elem c ['>','<',' ','v','^','@'] 
      -> controlFlow (Parser.Grid grid w h) s (dx,dy) line column
    c | Lst.elem c ['0'..'9']
      -> step (Parser.Grid grid w h) (pushInt c s) (dx,dy)
      (line+dy) (column+dx)
    c | Lst.elem c ['+']
      -> arithm (Parser.Grid grid w h) s (dx,dy) line column
    c | Lst.elem c ['.']
      -> user (Parser.Grid grid w h) s (dx,dy) line column
    _ -> putStrLn "error, instruction not implemented"

run :: Parser.Grid -> IO()
run grid = 
  step grid emptyStack (1,0) 0 0
