{-# LANGUAGE FlexibleInstances #-}

module Check where

import Math.Polynomial
import Math.Core.Field (F2)
import Control.Monad
import Numeric
import Utils
import Classes

import F16

fromF2 :: F2 -> Int
fromF2 0 = 0
fromF2 _ = 1

instance ToBits (Poly F2) where
  toBits p = map fromF2 (polyCoeffs LE p)

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

checkPowersF16 :: Int -> F16 -> IO Bool
checkPowersF16 n a@(F16 x) = 
  let p = poly LE (map fromIntegral (toCoeffs x))
      pows1 = take n $ iterate (*a) 1
      op q = remPoly (multPoly p q) r16
      pows2 = take n $ iterate op one
  in checkList pows1 pows2

checkList :: (Show a, ToBits a) => [a] -> [Poly F2] -> IO Bool
checkList as ps =
  let fails = [ t | t@(k,ak,pk) <- zip3 [(0::Int)..] as ps, not (checkBits ak pk) ]
  in case fails of
       [] -> return True
       ((k,ak,pk):_) -> do
         putStrLn $ "Test failed for k = " ++ show k
         putStrLn $ "  F16:  " ++ show ak
         putStrLn $ "  Poly: " ++ showPoly pk
         return False

