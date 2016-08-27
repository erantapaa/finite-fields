
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

type FFInt = Integer -- underlying int type used to represent the bits
newtype F16 = F16 FFInt

bitsPerCoeff = 5
ones = iterate (\x -> (shiftL x bitsPerCoeff) .|. 1) 0

masklo = ones !! 16
maskhi = shiftL masklo (16*bitsPerCoeff)

instance Eq F16 where
  F16 a == F16 b = a == b

reduce :: FFInt -> FFInt
reduce z = if zz == 0 then (z .&. masklo) else reduce x
  where zz = z .&. maskhi
        r1 = shiftR zz ((16-5)*bitsPerCoeff)
        r2 = shiftR zz ((16-3)*bitsPerCoeff)
        r3 = shiftR zz ((16-1)*bitsPerCoeff)
        r4 = shiftR zz (16*bitsPerCoeff)
        r = xor r1 (xor r2 (xor r3 r4))
        x = (xor (xor z zz) r) .&. (maskhi .|. masklo)

instance Num F16 where
  (+) (F16 a) (F16 b) = F16 ((a+b) .&. masklo)
  (*) (F16 a) (F16 b) = F16 (reduce (a*b))
  (-)                 = (+)
  negate              = id
  abs = error "abs not implemented for F16"
  signum = error "signum not implemented for F16"
  fromInteger a = F16 (foldl' go 0 [0..15])
    where go s k = if testBit a k then s .|. (bit (k*bitsPerCoeff)) else s

instance Fractional F16 where
  recip (F16 0) = error "division by zero"
  recip x = x ^ 65534
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

-- find generator
findgen = [ g | g <- map fromInteger [1..32767], let o = order g, o == 65535 ]

order :: F16 -> Int
order g = 1 + (length $ takeWhile test $ iterate (*g) g)
  where test 0 = False -- should never happen
        test 1 = False
        test _ = True

