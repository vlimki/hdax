{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Frame (readCsv, Frame, Value, col, row, Series) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector as V
import GHC.Generics
import Text.Read (readMaybe)

data Value = VInt Int | VDouble Double | VString String | VBool Bool
  deriving (Show, Eq, Generic)

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

class CsvField a where
  convert :: Value -> a

instance CsvField Int where
  convert (VInt x) = x
  convert _ = error "Invalid conversion"

instance CsvField Double where
  convert (VDouble x) = x
  convert _ = error "Invalid conversion"

instance CsvField String where
  convert (VString x) = x
  convert _ = error "Invalid conversion"

instance CsvField Bool where
  convert (VBool x) = x
  convert _ = error "Invalid conversion"

type Rec = M.HashMap T.Text Value

data Frame = Frame
  { headers :: V.Vector T.Text
  , records :: V.Vector Rec
  }
  deriving (Show, Eq)

type Series a = V.Vector a

col :: (CsvField a) => T.Text -> Frame -> Series a
col c (Frame _ recs) = V.map (Frame.convert . fromMaybe (error "Invalid column") . M.lookup c) recs

row :: Int -> Frame -> Rec
row idx (Frame _ r) = r V.! idx

readCsv :: String -> IO Frame
readCsv file = do
  csvData <- BL.readFile file

  case Csv.decodeByName csvData of
    Left err -> error err
    Right (h, v) -> return Frame{records = v, headers = V.map TE.decodeUtf8 h}
