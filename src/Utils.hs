
module Utils where

import Data.List

showBitsAsPoly :: [Int] -> String
showBitsAsPoly bits = intercalate " + " [ term i | (i,c) <- zip [0..] bits, c /= 0 ]
  where term :: Int -> String
        term 0 = "1"
        term k = "x^" ++ show k

