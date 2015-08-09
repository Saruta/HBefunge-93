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

-- grid :
--      * width  <-> length $ grid !! 0
--      * height <-> length grid
parse :: [Char] -> Grid
parse txt = 
  let grid = lines txt 
    in Grid 
      (Data.Vector.generate (length grid) 
        (\n -> Data.Vector.fromList $ grid !! n)) 
      (length $ grid !! 0)
      (length grid)

