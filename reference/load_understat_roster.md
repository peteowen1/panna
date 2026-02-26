# Load Understat Roster Data

Loads player statistics from Understat matches. Includes xG, xA, xG
chain, xG buildup, and other advanced metrics.

## Usage

``` r
load_understat_roster(
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

Data frame of player statistics from Understat.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all Understat roster data from remote
all_roster <- load_understat_roster()

# Load England 2024 only
eng_2024 <- load_understat_roster(league = "ENG", season = "2024")

# Load from local files
local_data <- load_understat_roster(source = "local")
} # }
```
