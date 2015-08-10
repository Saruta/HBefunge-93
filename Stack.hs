--
-- Stack abstract data type
--

module Stack where

import Data.List
import Data.Char

newtype Stack a = Stack [a] 

push :: a -> Stack a -> Stack a
push x (Stack s) = Stack (x:s)

pushInt :: Char -> Stack Int -> Stack Int
pushInt c s = 
  let x = digitToInt c
    in
      push x s

pushChar :: Char -> Stack Int -> Stack Int
pushChar c s = 
  let x = ord c
    in
      push x s

top :: Stack a -> a
top (Stack s) = head s

pop :: Stack a -> (a, Stack a)
pop (Stack (hd:s)) = (hd,Stack s)

emptyStack :: Stack a 
emptyStack = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack s) = null s

rmTop :: Stack a -> Stack a
rmTop (Stack (hd:s)) = Stack s

dupTop :: Stack a -> Stack a
dupTop s = push (top s) s

permTop :: Stack a -> Stack a
permTop s = let (x,s') = pop s in 
              let (x',s'') = pop s' in
                push x' $ push x s''

notTop :: Stack Int -> Stack Int
notTop s = let (x,s') = pop s in 
              if x == 0 then
                push 1 s'
              else
                push 0 s'

greatTop :: Stack Int -> Stack Int
greatTop s = let (x,s') = pop s in
                let (x',s'') = pop s' in
                  if x' > x then
                    push 1 s''
                  else
                    push 0 s''
