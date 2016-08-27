
module Main where

import Criterion
import Criterion.Main
import Classes

import F16
import F16a

test1 n =  1 == last ( take n $ iterate (* (3::F16))  (if n > 0 then 1 else 2))
test1a n = 1 == last ( take n $ iterate (* (3::F16a)) (if n > 0 then 1 else 2))

test2  n = toBits $ (5::F16) ^ n
test2a n = toBits $ (5::F16a) ^ n

test3 n = let a = (5::F16)
              go 0 s = s
              go k s = go (k-1) (s*a)
          in go n 1 == 1

test3a n = let a = (5::F16a)
               go 0 s = s
               go k s = go (k-1) (s*a)
           in go n 1 == 1

main = defaultMain [
  bgroup "f16"
    [ bench "Integer 1" $ nf test1  65535
    , bench "Word180 1" $ nf test1a 65535
    , bench "Integer 2" $ nf test2  65535
    , bench "Word180 2" $ nf test2a 65535
    , bench "Integer 3" $ nf test3  65535
    , bench "Word180 3" $ nf test3a 65535
    ]
  ]

