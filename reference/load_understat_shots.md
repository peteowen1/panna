# Load Understat Shots Data

Loads shot-level data with xG from Understat matches. Each row
represents a single shot with coordinates, xG, and outcome.

## Usage

``` r
load_understat_shots(
  league = NULL,
  season = NULL,
  source = c("remote", "local")
)
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "RUS").

- season:

  Optional season filter (e.g., "2024" or 2024). Note: Understat uses
  single year format, not YYYY-YYYY.

- source:

  Data source: "remote" (default) downloads from GitHub, "local" uses
  local parquet/RDS files.

## Value

Data frame of shot data from Understat.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all Understat shots from remote
all_shots <- load_understat_shots()

# Load Spain 2023 only
esp_2023 <- load_understat_shots(league = "ESP", season = "2023")
} # }
```
