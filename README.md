# hdax - Haskell Data Analysis eXtension

The goal is to make a dataframe/data analysis library that has seamless integration with a library like hmatrix, so it could be used for machine learning purposes. As of now, this is primarily a learning project and should not be used for production.

## Example
Say we have a CSV file like this (data/example.csv):

```csv
name,salary,age
John,123123,56
Mary,56000,32
Erich,3200,29
Philipp,,27
```

### Reading data into a DataFrame
```haskell
>>> df <- readCsv "data/example.csv"
>>> df
name     salary  age
John     123123  56
Mary     56000   32
Erich    3200    29
Philipp          27
```

### Columns
```haskell 
-- NOTE: The type for the `col` function must ALWAYS be explicitly specified.
>>> col "name" df :: Series String
["John", "Mary", "Erich", "Philipp"]
```

```haskell
>>> drop ["name", "salary"] df
age
56
32
29
27
```

Note that the column operations only work when there are no missing values. `hdax` will throw an error if you try to run any of these on a column with missing values.

```haskell
-- Note `Double` infers the type of the `mean` function, and not the type of `col "age" df`.
>>> mean $ col "age" $ rows [0..2] df :: Double

39.0
```

```haskell
>>> median $ col "salary" $ rows [0..2] df :: Double
56000.0
```

### Indexing
```haskell
>>> df !> 0 -- or `row 0 df`
Record { salary: 123123, name: John, age: 56 }
```

### Dealing With Missing Values
You can remove every element with missing values with the `dropna` function:
```haskell
>>> dropna "salary" df
name   salary  age
John   123123  56
Mary   56000   32
Erich  3200    29
```
You can also fill every missing value in a column with some predetermined value:
```haskell
>>> m = mean $ col "salary" df :: Double
>>> fillna "salary" m df
name     salary    age
John     123123    56
Mary     56000     32
Erich    3200      29
Philipp  45580.75  27
```

### Binning
```haskell
>>> bin "age" [(>30), (<30)] ["over30", "under30"] df
name     salary  age  over30  under30 
John     123123  56   1.0     0.0
Mary     56000   32   1.0     0.0
Erich    3200    29   0.0     1.0
Philipp          27   0.0     1.0
```

### One-Hot Encoding
```haskell
>>> encode "name" df
name     salary  age  name_John  name_Mary  name_Erich  name_Philipp
John     123123  56   1.0        0.0        0.0         0.0
Mary     56000   32   0.0        1.0        0.0         0.0
Erich    3200    29   0.0        0.0        1.0         0.0
Philipp          27   0.0        0.0        0.0         1.0
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
