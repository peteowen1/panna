# Validate data frame input

Validates that input is a non-empty data frame with required columns.
Throws informative errors using cli package when validation fails.

## Usage

``` r
validate_dataframe(data, required_cols = NULL, arg_name = "data", min_rows = 1)
```

## Arguments

- data:

  Data frame to validate

- required_cols:

  Character vector of required column names

- arg_name:

  Name of the argument (for error messages)

- min_rows:

  Minimum number of rows required (default 1)

## Value

TRUE invisibly if valid, otherwise throws an error

## Examples

``` r
df <- data.frame(player_name = "Messi", minutes = 90)
validate_dataframe(df, c("player_name", "minutes"))
# Returns TRUE

if (FALSE) { # \dontrun{
# These will throw errors:
validate_dataframe(NULL, c("player_name"))
validate_dataframe(data.frame(), c("player_name"))
validate_dataframe(df, c("nonexistent_col"))
} # }
```
