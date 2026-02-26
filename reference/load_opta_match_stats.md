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

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all match stats
ms <- load_opta_match_stats()

# Use with player_skill_profile for full diagnostic output
player_skill_profile("H. Kane", match_stats = ms)
} # }
```
