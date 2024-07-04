{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, ConstraintKinds, TypeFamilies, MultiParamTypeClasses #-}

module Frame
    ( Frame
    , Column
    , Value
    , createFrame
    ) where

import qualified Data.Vector as V
import Data.Dynamic
import qualified Data.Text as T
import Data.List (transpose)

class (Show a, Typeable a) => Value a
instance Value Double
instance Value T.Text

data Column = forall a. Value a => Column { name :: T.Text, values :: V.Vector a }
instance Show Column where
    show (Column n v) = unlines $ T.unpack n : V.toList (V.map show v)

newtype Frame = Frame [Column]
instance Show Frame where
    show (Frame cols) = unlines $ (map unwords . transpose) $ map printTranspose cols

printTranspose :: Column -> [String]
printTranspose (Column n v) = T.unpack n : V.toList (V.map show v)

createFrame :: (Value a) => [(T.Text, V.Vector a)] -> Frame
createFrame = Frame . Prelude.map (\(n, v) -> Column {name=n, values=v} )
