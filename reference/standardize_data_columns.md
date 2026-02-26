# Standardize data frame column names using a mapping

Renames columns in a data frame or data.table if alternative names
exist. This handles common variations in column naming across different
data sources. For data.table objects, uses setnames() for efficient
in-place renaming.

## Usage

``` r
standardize_data_columns(data, col_map)
```

## Arguments

- data:

  Data frame or data.table to standardize

- col_map:

  Named list where names are canonical column names and values are
  character vectors of alternative names to look for.

## Value

Data frame/data.table with standardized column names
