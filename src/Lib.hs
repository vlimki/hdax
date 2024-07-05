{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Lib (Frame, readCsv, CsvFrame, Record, get, Passenger) where

-- Write a proper test suite later instead of doing stuff here
import Frame
import GHC.Generics

--data Person = Person
--    { name :: String
--    , salary :: Double
--    , age :: Double
--    } deriving (Show, Generic, Record)

data Passenger = Passenger {
    passengerId :: Double,
    survived :: Double,
    pclass :: Double,
    name :: String,
    sex :: String,
    age :: Maybe Double,
    sibSp :: Double,
    parch :: Double,
    ticket :: !String,
    fare :: !Double,
    cabin :: Maybe String,
    embarked :: !String
} deriving (Show, Generic, Record)

--main :: IO ()
--main = do
--    df <- readCsv "data/example.csv" :: CsvFrame Person

    -- ["John","Mary","Erich]
--    let _names = get name df 
--    return ()
