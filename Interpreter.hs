--
-- Interpreter
-- 
module Interpreter where

import qualified Parser
import Stack
import Data.Vector
import Data.Char
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
    '#' -> step (Parser.Grid grid w h) s (dx,dy) (line+2*dy) 
      (column+2*dx)
    '_' -> let (x,s') = pop s in
      if x == 0 then 
        step (Parser.Grid grid w h) s' (1,0) line (column+1)
      else
        step (Parser.Grid grid w h) s' (-1,0) line (column-1) 
    '|' -> let (x,s') = pop s in
      if x == 0 then 
        step (Parser.Grid grid w h) s' (0,1) (line+1) column
      else
        step (Parser.Grid grid w h) s' (0,-1) (line-1) column 
    '@' -> putStrLn "--exit success--"
    

-- arithmetic
arithm :: Parser.Grid -> (Stack Int) -> (Int,Int) -> Int -> Int -> IO()
arithm (Parser.Grid grid w h)  s  (dx,dy) line column =
  case ((grid ! line)! column) of
    '+' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (x+y) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
    '-' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (y-x) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
    '*' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (x*y) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
    '/' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (y `quot` x) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)
    '%' -> let (x,s') = pop s in let (y,s'') = pop s' in
      let s''' = push (y `mod` x) s'' in
        step (Parser.Grid grid w h) s''' (dx,dy) (line+dy) (column+dx)


user :: Parser.Grid -> (Stack Int) -> (Int,Int) -> Int -> Int -> IO()
user (Parser.Grid grid w h) s (dx,dy) line column = 
  case ((grid ! line)! column) of
    '.' -> let (x,s') = pop s in
      do {print $ x;
      step (Parser.Grid grid w h) s' (dx,dy) (line+dy) (column+dx)}
    ',' -> let (x,s') = pop s in
      do {putChar $ chr x;
      step (Parser.Grid grid w h) s' (dx,dy) (line+dy) (column+dx)}
    '&' -> 
      do {x <- readLn;
      step (Parser.Grid grid w h) (push x s) (dx,dy) (line+dy) 
        (column+dx)}
    '~' -> 
      do {x <- getChar;
      step (Parser.Grid grid w h) (pushChar x s) (dx,dy) (line+dy) 
        (column+dx)}

stack :: Parser.Grid -> (Stack Int) -> (Int,Int) -> Int -> Int-> IO()
stack (Parser.Grid grid w h) s (dx,dy) line column = 
  case ((grid ! line)! column) of
    '$' -> step (Parser.Grid grid w h) (rmTop s) (dx,dy) (line+dy) 
        (column+dx) 
    '!' -> step (Parser.Grid grid w h) (notTop s) (dx,dy) (line+dy)
        (column+dx)
    '`' -> step (Parser.Grid grid w h) (greatTop s) (dx,dy) (line+dy)
        (column+dx)
    ':' -> step (Parser.Grid grid w h) (dupTop s) (dx,dy) (line+dy)
        (column+dx)
    '\\' -> step (Parser.Grid grid w h) (permTop s) (dx,dy) (line+dy)
        (column+dx)

string :: Parser.Grid -> (Stack Int) -> (Int,Int) -> Int -> Int ->IO()
string (Parser.Grid grid w h) s (dx,dy) line column =
  case ((grid ! line)! column) of
    '"' -> step (Parser.Grid grid w h) s (dx,dy) 
      (line+dy) (column+dx)
    c -> string (Parser.Grid grid w h) (pushChar c s) (dx,dy)   
      (line+dy) (column+dx)

-- program update --
_update ::  Vector (Vector Char)-> Int -> Int -> Char -> 
  Vector (Vector Char)
_update grid line column value = 
  let line_ = grid !line in
    let line_' = line_ // [(column,value)] in
      grid // [(line,line_')]

--        grid                    sens         line  column
step :: Parser.Grid -> (Stack Int) -> (Int,Int) -> Int -> Int -> IO()
step (Parser.Grid grid w h) s (dx,dy) line column = 
  let line_ = line `mod` h
      column_ = column `mod` w in
  case ((grid ! line_)! column_) of
    c | Lst.elem c ['>','<',' ','v','^','@','_','|','#'] 
      -> controlFlow (Parser.Grid grid w h) s (dx,dy) line_ column_
    c | Lst.elem c ['0'..'9']
      -> step (Parser.Grid grid w h) (pushInt c s) (dx,dy)
      (line_+dy) (column_+dx)
    c | Lst.elem c ['+','-','*','/','%']
      -> arithm (Parser.Grid grid w h) s (dx,dy) line_ column_
    c | Lst.elem c ['.',',','~','&']
      -> user (Parser.Grid grid w h) s (dx,dy) line_ column_
    c | Lst.elem c ['$','!',':','`','\\']
      -> stack (Parser.Grid grid w h) s (dx,dy) line_ column_
    'g' -> {- get -} let (y,s') = pop s in let (x,s'') = pop s' in
      if y > h || x > w then
        step (Parser.Grid grid w h) (push 0 s'') (dx,dy) (line_+dy)
          (column_+dx)
      else
        let p = ((grid! y)! x) in
          step (Parser.Grid grid w h) (pushChar p s'') (dx,dy) 
            (line_+dy) (column_ +dx)
    'p' -> {- put -} let (y,s') = pop s in let (x,s'') = pop s' in
      let (v,s''') = pop s'' in 
        let v' = chr v in
          if y > h || x > w then
            putStrLn "Error, put out of bounds"
          else
            step (Parser.Grid (_update grid y x v') w h) 
              s'' (dx,dy) (line_+dy) (column_+dx) 
    '"' -> string (Parser.Grid grid w h) s (dx,dy) (line_+dy) 
        (column_+dx)
    _ -> putStrLn "error, instruction not implemented"

run :: Parser.Grid -> IO()
run grid = let localStack = emptyStack in 
  step grid (push 0 localStack) (1,0) 0 0
