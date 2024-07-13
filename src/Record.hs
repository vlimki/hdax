{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Record (Record (..), convert, Value, CsvField, valueToR, field, fieldMaybe, toValue, isNull, set) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Csv as Csv
import Data.HashMap.Strict as M
import Data.Hashable
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics hiding (R)
import Numeric.LinearAlgebra (R)
import Text.Read (readMaybe)

newtype Record = Record {inner :: M.HashMap T.Text Value} deriving (Eq)

instance Show Record where
  show (Record i) = T.unpack $ "Record { " <> T.intercalate ", " formattedKvPairs <> " }"
    where
      kvPairs :: [(T.Text, T.Text)]
      kvPairs = Prelude.zip (M.keys i) (Prelude.map (T.pack . (convert @String)) $ M.elems i)
      formattedKvPairs = Prelude.map (\(k, v) -> k <> ": " <> v) kvPairs

instance Csv.FromNamedRecord Record where
  parseNamedRecord r = Record <$> Csv.parseNamedRecord r

field :: (CsvField a) => T.Text -> Record -> a
field s r = Record.convert $ fromMaybe (error "Invalid field") $ M.lookup s $ inner r

set :: (CsvField a) => T.Text -> a -> Record -> Record
set k v (Record i) = Record{inner = M.insert k (toValue v) i}

isNull :: T.Text -> Record -> Bool
isNull s r = case fromMaybe (error "Invalid field") $ M.lookup s (inner r) of
  VNone -> True
  _ -> False

fieldMaybe :: (CsvField a) => T.Text -> Record -> Maybe a
fieldMaybe s r = case M.lookup s $ inner r of
  Just v -> case v of
    VNone -> Nothing
    x -> Just $ convert x
  Nothing -> Nothing

class CsvField a where
  convert :: Value -> a
  toValue :: a -> Value

instance CsvField Int where
  convert (VInt x) = x
  convert VNone = 0
  convert _ = error "Invalid conversion"
  toValue = VInt

instance CsvField Double where
  convert (VDouble x) = x
  convert (VInt x) = fromIntegral x
  convert VNone = 0
  convert _ = error "Invalid conversion"
  toValue = VDouble

instance CsvField String where
  convert (VString x) = x
  convert (VInt x) = show x
  convert (VDouble x) = show x
  convert (VBool x) = show x
  convert VNone = ""
  toValue = VString

instance CsvField Bool where
  convert (VBool x) = x
  convert VNone = False
  convert _ = error "Invalid conversion"
  toValue = VBool

data Value = VInt Int | VDouble Double | VString String | VBool Bool | VNone
  deriving (Eq, Generic)

instance Show Value where
  show (VDouble x) = show x
  show (VInt x) = show x
  show (VString x) = x
  show (VBool x) = show x
  show VNone = ""

instance Hashable Value

instance Csv.FromField Value where
  parseField bs = parseAsInt bs <|> parseAsDouble bs <|> parseAsBool bs <|> parseAsString bs
    where
      parseAsInt :: BS.ByteString -> Csv.Parser Value
      parseAsInt s = case readMaybe (BSC.unpack s) of
        Just x -> pure (VInt x)
        _ -> mzero

      parseAsDouble :: BS.ByteString -> Csv.Parser Value
      parseAsDouble s = case readMaybe (BSC.unpack s) of
        Just x -> pure (VDouble x)
        _ -> mzero

      parseAsBool :: BS.ByteString -> Csv.Parser Value
      parseAsBool s = case readMaybe (BSC.unpack s) of
        Just x -> pure (VBool x)
        _ -> mzero

      parseAsString :: BS.ByteString -> Csv.Parser Value
      parseAsString s = pure (if val == "" then VNone else VString val)
        where
          val = BSC.unpack s

valueToR :: Value -> R
valueToR v = case v of
  VInt x -> fromIntegral x
  VDouble x -> x
  VString x -> read x
  VBool x -> if x then 1.0 else 0.0
  VNone -> error "Empty values. Maybe will just set them to 0 in the future"
