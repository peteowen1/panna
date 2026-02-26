# Load Understat Metadata

Loads match metadata from Understat including teams, scores, and xG
totals.

## Usage

``` r
load_understat_metadata(
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

Data frame of match metadata from Understat.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all Understat metadata from remote
all_meta <- load_understat_metadata()

# Load Germany 2024 only
ger_2024 <- load_understat_metadata(league = "GER", season = "2024")
} # }
```
