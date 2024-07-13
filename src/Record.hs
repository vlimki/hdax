{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Record (Record (..), convert, Value, CsvField, valueToR, field, fieldMaybe) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Csv as Csv
import Data.HashMap.Strict as M
import Data.Hashable
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics hiding (R)
import Numeric.LinearAlgebra (R)
import Text.Read (readMaybe)

newtype Record = Record {inner :: M.HashMap T.Text Value} deriving (Show, Eq)

instance Csv.FromNamedRecord Record where
  parseNamedRecord r = Record <$> Csv.parseNamedRecord r

field :: (CsvField a) => T.Text -> Record -> a
field s r = Record.convert $ fromMaybe (error "Invalid field") $ M.lookup s $ inner r

fieldMaybe :: (CsvField a) => T.Text -> Record -> Maybe a
fieldMaybe s r = fmap Record.convert $ M.lookup s $ inner r

class CsvField a where
  convert :: Value -> a

instance CsvField Int where
  convert (VInt x) = x
  convert _ = error "Invalid conversion"

instance CsvField Double where
  convert (VDouble x) = x
  convert (VInt x) = fromIntegral x
  convert _ = error "Invalid conversion"

instance CsvField String where
  convert (VString x) = x
  convert (VInt x) = show x
  convert (VDouble x) = show x
  convert (VBool x) = show x

instance CsvField Bool where
  convert (VBool x) = x
  convert _ = error "Invalid conversion"

data Value = VInt Int | VDouble Double | VString String | VBool Bool
  deriving (Eq, Generic)

instance Show Value where
  show (VDouble x) = show x
  show (VInt x) = show x
  show (VString x) = x
  show (VBool x) = show x

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
      parseAsString s = pure (VString $ BSC.unpack s)

valueToR :: Value -> R
valueToR v = case v of
  VInt x -> fromIntegral x
  VDouble x -> x
  VString x -> read x
  VBool x -> if x then 1.0 else 0.0
