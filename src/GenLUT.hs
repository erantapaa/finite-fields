
-- Generate exponential and log lookup tables
module GenLUT where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import F16
import Control.Monad
import Control.Monad.ST
import Classes

genExpLogTables :: Int -> F16 -> (UV.Vector Int, UV.Vector Int)
genExpLogTables size g = runST $ do
  logtable <- UVM.new size
  exptable <- UVM.new size
  let pairs = take size $ iterate (\(k,a) -> (k+1,a*g)) (0,1)
  forM_ pairs $ \(k,a) -> do
    let i = toInt a
    UVM.write exptable k i 
    UVM.write logtable i k
  exps <- UV.freeze exptable
  logs <- UV.freeze logtable
  return (exps,logs)

