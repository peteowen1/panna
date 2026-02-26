# Build SQL WHERE clause from filters

Constructs a SQL WHERE clause from a named list of filter conditions.
Values are properly quoted for SQL safety. NULL values are skipped.

## Usage

``` r
build_where_clause(filters, prefix = TRUE)
```

## Arguments

- filters:

  Named list where names are column names and values are filter values.
  Values can be character (quoted), numeric (unquoted), or NULL
  (skipped).

- prefix:

  Whether to include "WHERE " prefix. If FALSE, returns just the
  conditions joined by AND.

## Value

Character string with SQL WHERE clause, or empty string if no filters.

## Examples

``` r
if (FALSE) { # \dontrun{
build_where_clause(list(league = "ENG", season = "2023-2024"))
# Returns: "WHERE league = 'ENG' AND season = '2023-2024'"

build_where_clause(list(league = "ENG", min_goals = 5))
# Returns: "WHERE league = 'ENG' AND min_goals = 5"

build_where_clause(list(league = NULL, season = "2023-2024"))
# Returns: "WHERE season = '2023-2024'"

build_where_clause(list())
# Returns: ""
} # }
```
