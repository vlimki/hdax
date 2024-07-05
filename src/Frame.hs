{-# LANGUAGE RankNTypes, GADTs, DefaultSignatures, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Frame (Frame, (!>), readCsv, CsvFrame, get, Record) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding (Record)
import qualified Data.Vector as V
import GHC.Generics
import Data.Vector (toList)

class FromNamedRecord a => Record a where
  parseNamedRecord :: NamedRecord -> Parser a
  default parseNamedRecord :: (Generic a, GFromNamedRecord (Rep a)) => NamedRecord -> Parser a
  parseNamedRecord = genericParseNamedRecord defaultOptions

instance (Generic a, GFromNamedRecord (Rep a)) => FromNamedRecord a where
  parseNamedRecord = genericParseNamedRecord defaultOptions

data Frame a where
  Frame :: {records :: (FromNamedRecord a) => V.Vector a} -> Frame a

instance (Show a, FromNamedRecord a) => Show (Frame a)
    where show f = unlines $ toList $ V.map show (records f)

--type CsvFrame a = IO (Either String (Frame a))
type CsvFrame a = IO (Frame a)

(!>) :: (FromNamedRecord a) => Frame a -> Int -> a
(!>) f idx = records f V.! idx

get :: (FromNamedRecord a) => (a -> b) -> Frame a -> V.Vector b
get fn f = V.map fn $ records f

readCsv :: (FromNamedRecord a) => String -> IO (Frame a)
readCsv file = do
    csvData <- BL.readFile file

    case decodeByName csvData of
        Left err -> error err
        Right (_, v) -> return $ Frame {records=v}
