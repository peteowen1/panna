# Rename columns using a mapping

Renames columns in a data frame based on a named vector mapping. The
mapping format is: c(new_name1 = "old_name1", new_name2 = "old_name2")

## Usage

``` r
rename_columns(data, mapping)
```

## Arguments

- data:

  Data frame

- mapping:

  Named character vector where names are new column names and values are
  existing column names to rename

## Value

Data frame with renamed columns

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(a = 1, b = 2)
rename_columns(df, c(x = "a", y = "b"))
} # }
```
