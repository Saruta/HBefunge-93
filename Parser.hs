--
-- Lexical and Syntaxic analysis
--
module Parser where

import qualified Data.Vector 
import Data.List

-- array - width - length
data Grid = Grid 
  (Data.Vector.Vector (Data.Vector.Vector Char))
  Int -- Width
  Int -- Height

expand :: [Char] -> [Char]
expand line_ = line_ ++ (replicate (80 - (length line_)) ' ') 

-- grid :
--      * width  <-> length $ grid !! 0
--      * height <-> length grid
parse :: [Char] -> Grid
parse txt = 
  let grid = map expand $lines txt in
    let len = length grid in
      Grid 
        (Data.Vector.generate 25 
          (\n ->  if n < len then 
                    Data.Vector.fromList $ grid !! n
                  else 
                    Data.Vector.replicate 80 ' ')) 
            80 25

