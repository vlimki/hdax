{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Frame (readCsv, Frame, Value, col, row, Series, cols, rows, toHMatrix, (!>), field, Frame.filter, dropna, fillna) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Numeric.LinearAlgebra (Matrix, R, (><))
import Record
import Series
import Data.Function (on)
import Data.List (groupBy, sortBy)

data Frame = Frame
  { headers :: V.Vector T.Text
  , records :: V.Vector Record
  }
  deriving (Eq)

instance Show Frame where
  show (Frame h recs) =
    unwords (zipWith padRight longest $ V.toList $ V.map T.unpack h) ++ "\n" ++ unlines (V.toList $ V.map format recs)
    where
      format :: Record -> String
      format (Record m) = unwords $ zipWith (\(_, v) len -> padRight len (show v)) (sortCols $ zip (M.keys m) (M.elems m)) longest
      kvPairsForLongest = sortCols $ concatMap (\(Record x) -> M.toList x) recsInner
      recsInner = V.toList recs
      getColumnIndex x = fromMaybe (error "Not possible") $ V.elemIndex x h
      sortCols = sortBy (\(ka, _) (kb, _) -> compare (getColumnIndex ka) (getColumnIndex kb))
      longest :: [Int]
      longest = zipWith max headerLengths valueLengths
      headerLengths = map (length . T.unpack) $ V.toList h
      valueLengths = map (maximum . map (length . Record.convert @String . snd)) . groupBy ((==) `on` fst) $ kvPairsForLongest


padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str + 1) ' '

dropna :: T.Text -> Frame -> Frame
dropna c df@(Frame _ recs) = df{records=V.filter (isJust . fieldMaybe @String c) recs}

fillna :: (CsvField a) => T.Text -> a -> Frame -> Frame
fillna c v df@(Frame _ recs) = df{records=V.map (\x -> Record{inner=if isNothing (M.lookup c x) then M.insert c (toValue v) x else x}) $ V.map inner recs}

col :: (CsvField a) => T.Text -> Frame -> Series a
col c (Frame _ recs) = V.map (Record.convert . fromMaybe (error "Invalid column") . M.lookup c) $ V.map inner recs

cols :: [T.Text] -> Frame -> Frame
cols c (Frame _ recs) = Frame{headers = V.fromList c, records = V.map (\r -> Record{inner = r}) $ V.map (M.filterWithKey (\k _ -> k `elem` c)) $ V.map inner recs}

filter :: (Record -> Bool) -> Frame -> Frame
filter f df@(Frame _ recs) = df{records = V.filter f recs}

toHMatrix :: Frame -> Matrix R
toHMatrix (Frame h recs) = (c >< r) $ concat $ V.toList (V.map (map valueToR . M.elems) $ V.map inner recs) :: Matrix R
  where
    (r, c) = (V.length h, V.length recs)

row :: Int -> Frame -> Record
row idx (Frame _ r) = r V.! idx

rows :: [Int] -> Frame -> Frame
rows idxs df@(Frame _ r) = df{records=V.backpermute r $ V.fromList idxs}

(!>) :: Frame -> Int -> Record
(!>) = flip row

readCsv :: String -> IO Frame
readCsv file = do
  csvData <- BL.readFile file

  case Csv.decodeByName csvData of
    Left err -> error err
    Right (h, v) -> return Frame{records = v, headers = V.map TE.decodeUtf8 h}
