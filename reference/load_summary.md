# Load Summary Data

Loads player summary statistics from pannadata. Uses DuckDB to query
parquet files - filtering happens at query time.

## Usage

``` r
load_summary(league = NULL, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "GER").

- season:

  Optional season filter (e.g., "2023-2024").

- source:

  Data source: "remote" (default) downloads from GitHub, "local" uses
  local parquet files.

## Value

Data frame of player summary statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all summary data from remote
all_summary <- load_summary()

# Load England 2023-2024 only (efficient - filters in SQL)
eng_2023 <- load_summary(league = "ENG", season = "2023-2024")

# Load from local files
local_data <- load_summary(source = "local")
} # }
```
