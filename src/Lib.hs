{-# LANGUAGE DeriveGeneric #-}

module Lib (Frame, Record, Series, readCsv, col, row, Frame.filter) where

-- Write a proper test suite later instead of doing stuff here
import GHC.Generics
import Frame
import Record
import Series

data Person = Person
  { name :: String
  , salary :: Double
  , age :: Double
  }
  deriving (Show, Generic)

-- main :: IO ()
-- main = do
--    df <- readCsv "data/example.csv" :: CsvFrame Person

-- ["John","Mary","Erich]
--    let _names = get name df
--    return ()
