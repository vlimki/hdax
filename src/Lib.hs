{-# LANGUAGE DeriveGeneric #-}

module Lib (module Frame, module Record, module Series) where

-- Write a proper test suite later instead of doing stuff here

import Frame
import GHC.Generics
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
