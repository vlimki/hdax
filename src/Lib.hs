{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main (main) where

import Frame
import GHC.Generics (Generic)

data Person = Person
    { name :: String
    , salary :: Int
    , age :: Int
    } deriving (Show, Generic, Record)

main :: IO ()
main = do
    df <- readCsv "data/example.csv" :: CsvFrame Person

    -- ["John","Mary","Erich]
    let _names = get name df 
    return ()
