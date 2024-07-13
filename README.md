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

### Reading data into a DataFrame
```haskell
module Main (main) where

main :: IO ()
main = do
    -- Returns a `Frame` object
    df <- readCsv "data/example.csv"
    return ()
```

### Columns
```haskell 
-- NOTE: The type for the `col` function must ALWAYS be explicitly specified.
>>> col "name" df :: Series String
["John","Mary","Erich"]

```haskell
-- Note `Double` infers the type of the `mean` function, and not the type of `col "age" df`.
>>> mean $ col "age" df :: Double
39.0
```

```haskell
>>> median $ col "salary" df :: Double
56000.0
```

### Indexing
```haskell
>>> df !> 0 -- or `row 0 df`
Record { salary: 123123, name: John, age: 56 }
```

### `hmatrix` Integration
```haskell
-- This matrix can be fed directly to a machine learning model, for example.
>>> toHMatrix $ cols ["salary", "age"] df
(3><2)
 [ 123123.0, 56.0
 ,  56000.0, 32.0
 ,   3200.0, 29.0 ]
```
