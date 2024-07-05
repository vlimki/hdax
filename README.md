# hdax - Haskell Data Analysis eXtension

The goal is to make a dataframe/data analysis library that has seamless integration with a library like hmatrix, so it could be used for machine learning purposes. As of now, this is primarily a learning project and should not be used for production.

## Example
Say we have a CSV file like this (data/example.csv):

```csv
name,salary,age
John,123123,56
Mary,56000,32
Erich,3200,29
```

We can parse it and convert it to a dataframe with very little boilerplate:

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main (main) where

import Frame -- Gotta figure out the actual package uploading and module names later lol
import GHC.Generics (Generic)

data Person = Person
    { name :: String
    , salary :: Int
    , age :: Int
    } deriving (Show, Generic, Record)

main :: IO ()
main = do
    df <- readCsv "data/example.csv" :: CsvFrame Person
    return ()
```

```haskell 
>>> get name df 
["John","Mary","Erich"]
```

```haskell
>>> df !> 0
Person {name = "John", salary = 123123, age = 56}
```

```haskell
>>> name $ df !> 0
John
```
