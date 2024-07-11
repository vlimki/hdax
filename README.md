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
-- NOTE: The type for the `col` function must ALWAYS be explicitly specified
>>> col "name" df :: Series String
["John","Mary","Erich"]
```

### Indexing
```haskell
-- This returns a hashmap. Probably will have to implement something better soon.
>>> df !> 0
fromList [("salary",VInt 123123),("name",VString "John"),("age",VInt 56)]
```

### `hmatrix` Integration
```haskell
>>> toHMatrix $ cols ["salary", "age"] df
(3><2)
 [ 123123.0, 56.0
 ,  56000.0, 32.0
 ,   3200.0, 29.0 ]
```
