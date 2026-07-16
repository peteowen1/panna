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

## See also

Other opta loaders:
[`load_opta_big5()`](https://peteowen1.github.io/panna/reference/load_opta_big5.md),
[`load_opta_events()`](https://peteowen1.github.io/panna/reference/load_opta_events.md),
[`load_opta_fixtures()`](https://peteowen1.github.io/panna/reference/load_opta_fixtures.md),
[`load_opta_lineups()`](https://peteowen1.github.io/panna/reference/load_opta_lineups.md),
[`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md),
[`load_opta_match_stats()`](https://peteowen1.github.io/panna/reference/load_opta_match_stats.md),
[`load_opta_shot_events()`](https://peteowen1.github.io/panna/reference/load_opta_shot_events.md),
[`load_opta_shots()`](https://peteowen1.github.io/panna/reference/load_opta_shots.md),
[`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md),
[`load_opta_stats()`](https://peteowen1.github.io/panna/reference/load_opta_stats.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load EPL xmetrics
epl_xm <- load_opta_xmetrics("ENG", season = "2024-2025")

# Top xG players
head(epl_xm[order(-epl_xm$xg), c("player_name", "team_name", "xg", "goals")])
} # }
```
