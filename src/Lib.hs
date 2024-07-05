{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Lib (Frame, readCsv, CsvFrame, Record, get, Person) where

-- Write a proper test suite later instead of defining the Person struct here
import Frame
import GHC.Generics

data Person = Person
    { name :: String
    , salary :: Int
    , age :: Int
    } deriving (Show, Generic, Record)

--main :: IO ()
--main = do
--    df <- readCsv "data/example.csv" :: CsvFrame Person

    -- ["John","Mary","Erich]
--    let _names = get name df 
--    return ()
