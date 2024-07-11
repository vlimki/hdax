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
module Main (main) where

main :: IO ()
main = do
    -- Returns a `Frame` object
    df <- readCsv "data/example.csv"
    return ()
```

```haskell 
-- NOTE: The type for the `col` function must ALWAYS be explicitly specified
>>> col "name" df :: Series String
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

## Drawbacks
Only supports CSV fields in camelCase or snake_case.
