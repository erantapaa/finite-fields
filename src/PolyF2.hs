{-# LANGUAGE FlexibleInstances #-}

module PolyF2
  (fromF2, F2, module Math.Polynomial )

where

import Classes
import Math.Polynomial
import Math.Core.Field (F2)
import Data.List
import Data.Bits

fromF2 :: F2 -> Int
fromF2 0 = 0
fromF2 _ = 1

instance ToBits (Poly F2) where
  toBits p = map fromF2 (polyCoeffs LE p)

instance FromBits (Poly F2) where
  fromBits bs = poly LE (map toF2 bs)
    where toF2 0 = 0
          toF2 _ = 1

instance ToInt (Poly F2) where
  toInt p = foldl' go 0 ( zip [0..] (toBits p) )
    where go s (k,b) = if b == 1 then setBit s k else s

