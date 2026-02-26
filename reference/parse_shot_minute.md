# Parse shot minute strings

Converts shot minute strings like "90+4" into minute and added_time
components.

## Usage

``` r
parse_shot_minute(minute_str)
```

## Arguments

- minute_str:

  Character vector of minute strings (e.g., "45", "90+4", "45+1")

## Value

Data frame with columns: minute (numeric), added_time (numeric)
