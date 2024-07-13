module Series (Series, mean, median, length) where

import qualified Data.Vector as V
import Prelude hiding (length)

type Series a = V.Vector a

length :: Series a -> Int
length = V.length

mean :: (Fractional a) => Series a -> a
mean s = (1 / fromIntegral n) * V.sum s
  where n = length s

median :: Series a -> a
median s = s V.! (n `div` 2)
  where n = length s
