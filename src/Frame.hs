{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frame (module Record, module Series, readCsv, Frame, col, row, cols, rows, toHMatrix, (!>), Frame.filter, dropna, fillna, Frame.drop, bin, Frame.length, Frame.map, encode) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import Data.List (groupBy, sortBy)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Numeric.LinearAlgebra (Matrix, R, (><))
import Record
import Series

data Frame = Frame
  { headers :: V.Vector T.Text
  , records :: V.Vector Record
  }
  deriving (Eq)

instance Show Frame where
  show (Frame h recs) =
    init $ unwords (zipWith padRight longest $ V.toList $ V.map T.unpack h) ++ "\n" ++ unlines (V.toList $ V.map format recs)
    where
      format :: Record -> String
      format (Record m) = unwords $ zipWith (\(_, v) len -> padRight len (show v)) (sortCols $ zip (M.keys m) (M.elems m)) longest
      kvPairsForLongest = sortCols $ concatMap (\(Record x) -> M.toList x) recsInner
      recsInner = V.toList recs
      getColumnIndex x = fromMaybe (error "Not possible") $ V.elemIndex x h
      sortCols = sortBy (\(ka, _) (kb, _) -> compare (getColumnIndex ka) (getColumnIndex kb))
      longest :: [Int]
      longest = zipWith max headerLengths valueLengths
      headerLengths = Prelude.map (Prelude.length . T.unpack) $ V.toList h
      valueLengths = Prelude.map (maximum . Prelude.map (Prelude.length . Record.convert @String . snd)) . groupBy ((==) `on` fst) $ kvPairsForLongest

padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - Prelude.length str + 1) ' '

map :: (Record -> Record) -> Frame -> Frame
map f df@(Frame _ recs) = df{records=V.map f recs}

-- c = column name to bin
-- bins = functions that test if the column fills some predicate
-- hs = new bin column headers. length must match the length of `bins`
bin :: (CsvField a) => T.Text -> [a -> Bool] -> [T.Text] -> Frame -> Frame
bin c bins binHeaders (Frame hs recs) =
  Frame
    { headers = V.concat [hs, V.fromList binHeaders]
    , records = V.map (\r -> foldl (\r' (h, b) -> Record{inner = M.insert h (VDouble (if b (get c r') then 1.0 else 0.0)) (m r')}) r bhPairs) recs
    }
  where
    m = inner
    bhPairs = zip binHeaders bins

binValue :: T.Text -> [Value -> Bool] -> [T.Text] -> Frame -> Frame
binValue c bins binHeaders (Frame hs recs) =
  Frame
    { headers = V.concat [hs, V.fromList binHeaders]
    , records = V.map (\r -> foldl (\r' (h, b) -> Record{inner = M.insert h (VDouble (if b (getValue c r') then 1.0 else 0.0)) (m r')}) r bhPairs) recs
    }
  where
    m = inner
    bhPairs = zip binHeaders bins

drop :: [T.Text] -> Frame -> Frame
drop columns (Frame hs recs) = Frame{headers = V.filter (`notElem` columns) hs, records = dropHelper columns recs}
  where
    dropHelper :: [T.Text] -> V.Vector Record -> V.Vector Record
    dropHelper [c] rs = V.map (del c) rs
    dropHelper [] rs = rs
    dropHelper (c : cs) rs = dropHelper cs (V.map (del c) rs)

dropna :: T.Text -> Frame -> Frame
dropna c df@(Frame _ recs) = df{records = V.filter (isJust . fieldMaybe @String c) recs}

fillna :: (CsvField a) => T.Text -> a -> Frame -> Frame
fillna c v df@(Frame _ recs) = df{records = V.map (\x -> if isNull c x then set c v x else x) recs}

col :: (CsvField a) => T.Text -> Frame -> Series.Series a
col c (Frame _ recs) = V.map (Record.convert . fromMaybe (error "Invalid column") . M.lookup c) $ V.map inner recs

colValue :: T.Text -> Frame -> Series.Series Value
colValue c (Frame _ recs) = V.map (fromMaybe (error "Invalid column") . M.lookup c) $ V.map inner recs

-- V.map (\v -> (\a -> a == v)) values
encode :: T.Text -> Frame -> Frame
encode c df = binValue c (V.toList $ V.map (\v -> (== v)) values) (Prelude.map T.pack $ V.toList hs) df
  where
    values = V.filter (/= VNone) $ colValue c df
    hs = V.filter (/= "") $ V.map ((++) (T.unpack c ++ "_") . show) values

cols :: [T.Text] -> Frame -> Frame
cols c (Frame _ recs) = Frame{headers = V.fromList c, records = V.map (\r -> Record{inner = r}) $ V.map (M.filterWithKey (\k _ -> k `elem` c)) $ V.map inner recs}

filter :: (Record -> Bool) -> Frame -> Frame
filter f df@(Frame _ recs) = df{records = V.filter f recs}

length :: Frame -> Int
length (Frame _ recs) = V.length recs

toHMatrix :: Frame -> Matrix R
toHMatrix (Frame h recs) = (c >< r) $ concat $ V.toList (V.map (Prelude.map valueToR . M.elems) $ V.map inner recs) :: Matrix R
  where
    (r, c) = (V.length h, V.length recs)

row :: Int -> Frame -> Record
row idx (Frame _ r) = r V.! idx

rows :: [Int] -> Frame -> Frame
rows idxs df@(Frame _ r) = df{records = V.backpermute r $ V.fromList idxs}

(!>) :: Frame -> Int -> Record
(!>) = flip row

readCsv :: String -> IO Frame
readCsv file = do
  csvData <- BL.readFile file

  case Csv.decodeByName csvData of
    Left err -> error err
    Right (h, v) -> return Frame{records = v, headers = V.map TE.decodeUtf8 h}
