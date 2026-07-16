# Load pre-computed Opta match-level stats

Downloads and queries `opta_match_stats.parquet` from the `opta-latest`
GitHub release. This file contains processed match-level player stats
with `_p90` columns, produced by the estimated skills pipeline step 01
(~15 MB, ~100K rows).

## Usage

``` r
load_opta_match_stats(
  season = NULL,
  columns = NULL,
  source = c("remote", "local"),
  repo = "peteowen1/pannadata",
  tag = "opta-latest"
)
```

## Arguments

- season:

  Optional season filter as end year (e.g., `2025`).

- columns:

  Optional character vector of columns to select.

- source:

  Data source: `"remote"` (default) or `"local"`.

- repo:

  GitHub repository (default: "peteowen1/pannadata").

- tag:

  Release tag (default: "opta-latest").

## Value

Data frame with one row per player-match containing processed stats with
`_p90` suffixes, `player_id`, `player_name`, `match_date`,
`total_minutes`, etc.

## See also

Other opta loaders:
[`load_opta_big5()`](https://peteowen1.github.io/panna/reference/load_opta_big5.md),
[`load_opta_events()`](https://peteowen1.github.io/panna/reference/load_opta_events.md),
[`load_opta_fixtures()`](https://peteowen1.github.io/panna/reference/load_opta_fixtures.md),
[`load_opta_lineups()`](https://peteowen1.github.io/panna/reference/load_opta_lineups.md),
[`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md),
[`load_opta_shot_events()`](https://peteowen1.github.io/panna/reference/load_opta_shot_events.md),
[`load_opta_shots()`](https://peteowen1.github.io/panna/reference/load_opta_shots.md),
[`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md),
[`load_opta_stats()`](https://peteowen1.github.io/panna/reference/load_opta_stats.md),
[`load_opta_xmetrics()`](https://peteowen1.github.io/panna/reference/load_opta_xmetrics.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all match stats
ms <- load_opta_match_stats()

# Use with player_skill_profile for full diagnostic output
player_skill_profile("H. Kane", match_stats = ms)
} # }
```
