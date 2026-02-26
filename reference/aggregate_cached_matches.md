# Aggregate cached match data

Combines all cached data for a table type into a single data frame. Uses
parquet files if available (fast), falls back to RDS files (slower).

## Usage

``` r
aggregate_cached_matches(
  table_type,
  league = NULL,
  season = NULL,
  prefer_parquet = TRUE,
  source = c("local", "remote")
)
```

## Arguments

- table_type:

  Type of table to aggregate (e.g., "summary", "shots")

- league:

  Optional league filter (e.g., "ENG")

- season:

  Optional season filter (e.g., "2024-2025")

- prefer_parquet:

  If TRUE (default), use parquet when available

- source:

  "local" (default) or "remote" to download from GitHub releases

## Value

Combined data frame, or NULL if no cached data

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all cached summary stats (uses parquet if available)
all_summary <- aggregate_cached_matches("summary")

# Load only Premier League 2024-2025
pl_summary <- aggregate_cached_matches("summary", league = "ENG",
                                        season = "2024-2025")
} # }
```
