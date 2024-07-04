{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( readCsv
    ) where

import qualified Data.Vector as V
import System.IO
import Frame
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readCsv :: String -> IO Frame
readCsv fName = do
    handle <- openFile fName ReadMode
    contents <- TIO.hGetContents handle
    let header = T.splitOn "," $ head $ T.lines contents
    let r = tail $ T.lines contents

    let c = transpose $ map (T.splitOn ",") r
    return $ createFrame [(h, v) | (h, v) <- zip header (map V.fromList c)]
