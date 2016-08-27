{-# LANGUAGE FlexibleInstances #-}

module Check where

import Math.Polynomial
import Math.Core.Field (F2)
import Control.Monad
import Numeric
import Utils
import Classes
import Data.List
import PolyF2

import F16
import F16a

r16 = poly LE [(1::F2), 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]

-- polynomial power mod
polyPowerMod m p 0 = one
polyPowerMod m p k =
  let (q,r) = quotRem k 2
      s = polyPowerMod m p q
      s' = remPoly (multPoly s s) m
      s'' = if r == 0 then s' else remPoly (multPoly s' p) m
  in s''

showPoly :: Poly F2 -> String
showPoly p = showBitsAsPoly (map fromF2 (polyCoeffs LE p))

checkBits :: (ToBits a, ToBits b) => a -> b -> Bool
checkBits x p = toBits x == toBits p

checkPowersF16 :: (Show a, Num a, ToBits a) => Int -> a -> IO Bool
checkPowersF16 n a = 
  let p = poly LE (map fromIntegral (toBits a))
      pows1 = take n $ iterate (*a) 1
      op q = remPoly (multPoly p q) r16
      pows2 = take n $ iterate op one
  in checkList pows1 pows2

checkF16withF16a :: Int -> Integer -> IO Bool
checkF16withF16a n m =
  let a = fromInteger m :: F16
      b = fromInteger m :: F16a
      pows1 = take n $ iterate (*a) 1
      pows2 = take n $ iterate (*b) 1
  in checkList pows1 pows2

showlist :: Show a => [a] -> String
showlist xs = intercalate " " (map show xs)

checkList :: (Show a, ToBits a, Show b, ToBits b) => [a] -> [b] -> IO Bool
checkList as ps =
  let fails = [ t | t@(k,ak,pk) <- zip3 [(0::Int)..] as ps, not (checkBits ak pk) ]
  in case fails of
       [] -> return True
       ((k,ak,pk):_) -> do
         putStrLn $ "Test failed for k = " ++ show k
         putStrLn $ "  A: " ++ showlist (toBits ak)
         putStrLn $ "  B: " ++ showlist (toBits pk)
         return False

