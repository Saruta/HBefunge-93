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


