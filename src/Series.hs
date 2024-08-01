module Series (Series, mean, median, Series.sum, Series.max, Series.min, stddev, minMax, zScore) where

import Data.List (sort)
import qualified Data.Vector as V
import Prelude hiding (length)

type Series a = V.Vector a

mean :: (Fractional a) => Series a -> a
mean s = (1 / fromIntegral n) * V.sum s
  where
    n = V.length s

median :: (Ord a) => Series a -> a
median s = sort (V.toList s) !! (n `div` 2)
  where
    n = V.length s

sum :: (Num a) => Series a -> a
sum = V.sum

max :: (Ord a) => Series a -> a
max s = maximum $ V.toList s

min :: (Ord a) => Series a -> a
min s = minimum $ V.toList s

stddev :: (Floating a) => Series a -> a
stddev s = sqrt ((1/n) * V.sum (V.map (\x -> (x - mu) ** 2) s))
  where
    mu = mean s
    n = fromIntegral $ V.length s

minMax :: (Fractional a, Ord a) => Series a -> Series a
minMax s = V.map (\x -> (x - Series.min s) / (Series.max s - Series.min s)) s

zScore :: (Floating a) => Series a -> Series a
zScore s = V.map (\x -> (x - mu) / sigma) s
  where
    mu = mean s
    sigma = stddev s
