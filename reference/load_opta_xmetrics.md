# Load Opta xG/xA/xPass Player Metrics

Loads pre-computed player-level xG, xA, and xPass metrics from parquet
files. Remote mode downloads a consolidated file from GitHub Releases
(opta-latest). Local mode reads pipeline-generated per-league/season
files from disk.

## Usage

``` r
load_opta_xmetrics(
  league,
  season = NULL,
  columns = NULL,
  source = c("remote", "local"),
  by_match = FALSE
)
```

## Arguments

- league:

  League code (e.g., "ENG", "EPL").

- season:

  Optional season filter (e.g., "2024-2025").

- columns:

  Optional character vector of columns to select.

- source:

  Data source: "remote" (default, from GitHub Releases) or "local"
  (pipeline-generated files).

- by_match:

  Logical. If `TRUE`, load the per-player-match artifact
  (`xmetrics_bymatch/`, one row per player-match keyed by `match_id`)
  instead of the season-level aggregate. Default `FALSE`.

## Value

Data frame with player xmetrics including xg, npxg, xa, xpass stats.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load EPL xmetrics
epl_xm <- load_opta_xmetrics("ENG", season = "2024-2025")

# Top xG players
head(epl_xm[order(-epl_xm$xg), c("player_name", "team_name", "xg", "goals")])
} # }
```
