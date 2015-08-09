--
-- Syntax checker
--
module Syntax where

import Data.List

instructions = ['+','-','*','/','%','!','`','>','<','^','v','?',
  '_','|','"',':','\\',' ','.','$',',','#','@','~','&','g','p'] 
  ++ ['0'..'9']

isCorrectWidth :: [Char] -> Bool
isCorrectWidth str = False

isEmptyLine :: [Char]           -> Bool
isEmptyLine (hd:t) | hd == ' '  = True && isEmptyLine t
isEmptyLine (hd:t)   = False
isEmptyLine       []            = True 

isCorrectCharacter :: Char -> Bool
isCorrectCharacter c = elem c instructions

isCorrectLine :: [Char] -> Bool
isCorrectLine (hd:t) | isCorrectCharacter hd = True && isCorrectLine t 
isCorrectLine (hd:t)      = False
isCorrectLine []          = True

subChecker :: [[Char]] -> Int -> Bool
subChecker (hd:t) width | isEmptyLine hd = False
subChecker (hd:t) width | not $ isCorrectLine hd = False
subChecker (hd:t) width | width \= length hd = False
subChecker (hd:t) width = True && subChecker t width
subChecker [] width = True 

checker :: [Char] -> Bool
checker txt = 
  let grid = lines txt
    in
      subChecker grid $ length $ grid !! 0
