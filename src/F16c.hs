{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- based on ed kemmett's code at: https://www.schoolofhaskell.com/user/edwardk/parallel-crc

module F16c where

import Data.Bits
import Data.Word

-- show
newtype GF = GF { runGF :: Word32 } deriving (Eq,Show,Read,Bits)

-- 0,1,3,5,16
-- 1101 | 0100 | 0000 | 0000
-- D 4 0 0
poly :: GF
poly = GF 0xd400

-- | compute x * p(x)
xtimes :: GF -> GF
xtimes c = unsafeShiftR c 1 + if testBit c 0 then poly else 0

instance Num GF where
  (+) = xor
  (-) = xor
  _ * 0 = 0
  a * b = xtimes a * unsafeShiftL b 1 + if testBit b 15 then a else 0
  negate = id
  abs = id
  signum = fromIntegral . signum . runGF

