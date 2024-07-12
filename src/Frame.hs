{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Frame (readCsv, Frame, Value, col, row, Series, cols, toHMatrix, (!>), field, Frame.filter) where

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
import GHC.Generics hiding (R)
import Text.Read (readMaybe)
import Numeric.LinearAlgebra (Matrix, (><), R)
import Data.Csv (FromNamedRecord)

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
  convert (VInt x) = fromIntegral x
  convert _ = error "Invalid conversion"

instance CsvField String where
  convert (VString x) = x
  convert _ = error "Invalid conversion"

instance CsvField Bool where
  convert (VBool x) = x
  convert _ = error "Invalid conversion"

valueToR :: Value -> R
valueToR v = case v of
  VInt x -> fromIntegral x
  VDouble x -> x
  VString x -> read x
  VBool x -> if x then 1.0 else 0.0

newtype Record = Record { inner :: M.HashMap T.Text Value } deriving (Show, Eq)

instance FromNamedRecord Record   where
  parseNamedRecord r = Record <$> Csv.parseNamedRecord r

field :: (CsvField a) => T.Text -> Record -> a
field s r = Frame.convert $ fromMaybe (error "Invalid field") $ M.lookup s $ inner r

data Frame = Frame
  { headers :: V.Vector T.Text,
    records :: V.Vector Record
  }
  deriving (Show, Eq)

type Series a = V.Vector a

col :: (CsvField a) => T.Text -> Frame -> Series a
col c (Frame _ recs) = V.map (Frame.convert . fromMaybe (error "Invalid column") . M.lookup c) $ V.map inner recs

cols :: [T.Text] -> Frame -> Frame
cols c (Frame _ recs) = Frame { headers = V.fromList c, records = V.map (\r -> Record{inner=r}) $ V.map (M.filterWithKey (\k _ -> k `Prelude.elem` c)) $ V.map inner recs }

filter :: (Record -> Bool) -> Frame -> Frame
filter f (Frame h recs) = Frame{headers=h, records=V.filter f recs}

toHMatrix :: Frame -> Matrix R
toHMatrix (Frame h recs) = (c><r) $ Prelude.concat $ V.toList (V.map (Prelude.map valueToR . M.elems) $ V.map inner recs) :: Matrix R
  where
    (r, c) = (V.length h, V.length recs)

row :: Int -> Frame -> Record
row idx (Frame _ r) = r V.! idx

(!>) :: Frame -> Int -> Record
(!>) = flip row

readCsv :: String -> IO Frame
readCsv file = do
  csvData <- BL.readFile file

  case Csv.decodeByName csvData of
    Left err -> error err
    Right (h, v) -> return Frame {records = v, headers = V.map TE.decodeUtf8 h}
