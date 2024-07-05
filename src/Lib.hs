--{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Lib (Frame, readCsv, CsvFrame, Record, get) where

import Frame

--data Person = Person
--    { name :: String
--    , salary :: Int
--    , age :: Int
--    } deriving (Show, Generic, Record)

--main :: IO ()
--main = do
--    df <- readCsv "data/example.csv" :: CsvFrame Person

    -- ["John","Mary","Erich]
--    let _names = get name df 
--    return ()
