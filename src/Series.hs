module Series (Series, mean, median, length) where

import qualified Data.Vector as V
import Prelude hiding (length)
import Data.List (sort)

type Series a = V.Vector a

length :: Series a -> Int
length = V.length

mean :: (Fractional a) => Series a -> a
mean s = (1 / fromIntegral n) * V.sum s
  where n = length s

median :: (Ord a) => Series a -> a
median s = sort (V.toList s) !! (n `div` 2)
  where n = length s
