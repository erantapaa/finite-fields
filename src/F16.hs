
{-# LANGUAGE BangPatterns #-}

module F16 where

import Data.Int
import Data.Bits
import Data.List
import Data.Ratio
import Numeric
import Data.Ord
import Debug.Trace
import Utils
import Classes

type FFInt = Integer -- underlying int type used to represent the bits
newtype F16 = F16 FFInt

ndims = 16  -- dimension of the field
bitsPerCoeff = 5
ones = iterate (\x -> (shiftL x bitsPerCoeff) .|. 1) 0

masklo = ones !! ndims
maskhi = shiftL masklo (ndims*bitsPerCoeff)

instance Eq F16 where
  F16 a == F16 b = a == b

reduce :: FFInt -> FFInt
reduce z = if zz == 0 then (z .&. masklo) else reduce x
  where zz = z .&. maskhi
        r1 = shiftR zz ((ndims-5)*bitsPerCoeff)
        r2 = shiftR zz ((ndims-3)*bitsPerCoeff)
        r3 = shiftR zz ((ndims-1)*bitsPerCoeff)
        r4 = shiftR zz (ndims*bitsPerCoeff)
        r = xor r1 (xor r2 (xor r3 r4))
        x = (xor (xor z zz) r) .&. (maskhi .|. masklo)

instance Num F16 where
  (+) (F16 a) (F16 b) = F16 ((a+b) .&. masklo)
  (*) (F16 a) (F16 b) = F16 (reduce (a*b))
  (-)                 = (+)
  negate              = id
  abs = error "abs not implemented for F16"
  signum = error "signum not implemented for F16"
  fromInteger a = F16 (foldl' go 0 [0..ndims-1])
    where go s k = if testBit a k then s .|. (bit (k*bitsPerCoeff)) else s

instance Fractional F16 where
  recip (F16 0) = error "division by zero"
  recip x = x ^ (2^ndims-2 :: Int)
  fromRational r = fromInteger p * (recip (fromInteger q))
    where p = numerator r
          q = denominator r

instance Enum F16 where
  toEnum = fromIntegral
  fromEnum (F16 a) = go 1 0 a
    where go _  s 0 = s
          go !b !s a = if testBit a 1
                         then go (2*b) (s+b) (shiftR a bitsPerCoeff)
                         else go (2*b) s     (shiftR a bitsPerCoeff)

toCoeffs' :: F16 -> [Int]
toCoeffs' (F16 x) = toCoeffs x

toCoeffs :: FFInt -> [Int]
toCoeffs a = map fromEnum terms
  where
    terms = map go bits 
    bits = takeWhile (/=0) $ iterate (\x -> shiftR x bitsPerCoeff) a
    go a = testBit a 0

instance Show F16 where
  show (F16 a) = showBitsAsPoly (toCoeffs a)

instance ToBits F16 where
  toBits = toCoeffs'

instance FromBits F16 where
  fromBits bs = F16 x
    where x = foldl' go 0 (zip [0,bitsPerCoeff..]  bs)
          go s (k,0) = s
          go s (k,_) = setBit s k

instance ToInt F16 where
  toInt = genericToInt


