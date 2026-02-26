# Compute match-level Opta statistics with per-90 rates

Like
[`aggregate_opta_stats()`](https://peteowen1.github.io/panna/reference/aggregate_opta_stats.md)
but preserves individual player-match rows instead of aggregating across
matches. Each row gets per-90 rates and derived features computed from
that single match's data. Used as input for the estimated skills
pipeline where decay-weighted averaging replaces aggregation.

## Usage

``` r
compute_match_level_opta_stats(opta_stats, min_minutes = 10)
```

## Arguments

- opta_stats:

  Data frame from
  [`load_opta_stats()`](https://peteowen1.github.io/panna/reference/load_opta_stats.md),
  one row per player-match. Must contain `match_id`, `match_date`,
  `player_name`, `team_name`, `team_position`, and `minsPlayed`.

- min_minutes:

  Minimum minutes in a single match for inclusion (default 10). Filters
  out very short cameos where per-90 rates are unreliable.

## Value

A data.table with one row per player-match containing:

- Identity columns: match_id, match_date, player_id, player_name,
  team_name, position, competition, season

- Context columns: opponent_team, is_home

- Minutes: total_minutes

- Per-90 rate columns (same names as aggregate_opta_stats output)

- Derived efficiency columns (same names as aggregate_opta_stats output)

## Examples

``` r
if (FALSE) { # \dontrun{
opta_stats <- load_opta_stats("ENG", "2024-2025")
match_stats <- compute_match_level_opta_stats(opta_stats)
} # }
```
