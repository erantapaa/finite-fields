
module Classes where

class ToBits a where
  toBits :: a -> [Int]

class FromBits a where
  fromBits :: [Int] -> a

class ToInt a where
  toInt :: a -> Int

genericToInt :: ToBits a => a -> Int
genericToInt a = foldr (\b s -> 2*s+b) 0 (toBits a)

