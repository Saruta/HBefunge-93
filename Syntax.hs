--
-- dummy syntax checker
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

isCorrectLine :: [Char] -> Bool -> Bool
isCorrectLine (hd:t) inStr | hd == '"' = isCorrectLine t (not inStr) 
isCorrectLine (hd:t) inStr | inStr = isCorrectLine t inStr
isCorrectLine (hd:t) inStr | isCorrectCharacter hd = 
  isCorrectLine t inStr 
isCorrectLine (hd:t) inStr = False
isCorrectLine []     False = True

subChecker :: [[Char]] -> Int -> Bool
subChecker (hd:t) width | isEmptyLine hd = False
subChecker (hd:t) width | not $ isCorrectLine hd False = False
subChecker (hd:t) width | width /= length hd = False
subChecker (hd:t) width = True && subChecker t width
subChecker [] width = True 

checker :: [Char] -> Bool
checker txt = 
  let grid = lines txt
    in
      subChecker grid $ length $ grid !! 0
