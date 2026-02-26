# Check and report data completeness

Validates data quality and reports missing values.

## Usage

``` r
validate_data_completeness(data, required_cols = NULL, warn = TRUE)
```

## Arguments

- data:

  Data frame to validate

- required_cols:

  Character vector of required column names

- warn:

  Logical, whether to print warnings for missing data

## Value

List with validation results
