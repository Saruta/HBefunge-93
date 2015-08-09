--
-- Lexical and Syntaxic analysis
--
module Parser where

import qualified Data.Vector as V
-- array - width - length
-- data ParseInfo = ParseInfo V.Vector Int Int

getLineSize :: [Char] -> Int
getLineSize ('\n':t) = 0
getLineSize [] = 0
getLineSize (hd:t) = 1 + getLineSize t

getNumbLines :: [Char] -> Int
getNumbLines str = length $ lines str

maxLineSize :: [Char] -> Int
maxLineSize str = 42

parse :: [Char] -> Int 
parse str = length $ lines str 

