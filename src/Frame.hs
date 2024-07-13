{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Frame (readCsv, Frame, Value, col, row, Series, cols, toHMatrix, (!>), field, Frame.filter) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as M
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector as V
import Numeric.LinearAlgebra (Matrix, R, (><))
import Record
import Series

data Frame = Frame
  { headers :: V.Vector T.Text
  , records :: V.Vector Record
  }
  deriving (Eq)

instance Show Frame where
  show (Frame h recs) = unwords (V.toList $ V.map T.unpack h) Prelude.++ "\n" Prelude.++ unlines (V.toList $ V.map format recs)
    where
      format :: Record -> String
      format (Record m) = unwords $ Prelude.map (\(_, v) -> show v) $ sortCols $ Prelude.zip (M.keys m) (M.elems m)
      getColumnIndex x = fromMaybe (error "Not possible") $ elemIndex x h
      sortCols = sortBy (\(ka, _) (kb, _) -> compare (getColumnIndex ka) (getColumnIndex kb))

col :: (CsvField a) => T.Text -> Frame -> Series a
col c (Frame _ recs) = V.map (Record.convert . fromMaybe (error "Invalid column") . M.lookup c) $ V.map inner recs

cols :: [T.Text] -> Frame -> Frame
cols c (Frame _ recs) = Frame{headers = V.fromList c, records = V.map (\r -> Record{inner = r}) $ V.map (M.filterWithKey (\k _ -> k `Prelude.elem` c)) $ V.map inner recs}

filter :: (Record -> Bool) -> Frame -> Frame
filter f (Frame h recs) = Frame{headers = h, records = V.filter f recs}

toHMatrix :: Frame -> Matrix R
toHMatrix (Frame h recs) = (c >< r) $ Prelude.concat $ V.toList (V.map (Prelude.map valueToR . M.elems) $ V.map inner recs) :: Matrix R
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
    Right (h, v) -> return Frame{records = v, headers = V.map TE.decodeUtf8 h}
