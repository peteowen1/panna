# Generate a player's estimated skill profile

Returns a clean summary of a player's current estimated skills: per-stat
skill estimates, league averages, percentile ranks, and confidence
levels. Groups stats by category for easy interpretation.

## Usage

``` r
player_skill_profile(
  player_name,
  match_stats = NULL,
  decay_params = NULL,
  date = Sys.Date(),
  min_weighted_90s = 5,
  skills = NULL,
  source = c("remote", "local")
)
```

## Arguments

- player_name:

  Character string of the player name to profile. Supports exact names
  ("H. Kane"), abbreviations ("H.Kane"), surnames ("Kane"), and
  accent-insensitive input ("Mbappe").

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).
  If provided, runs full skill estimation from raw data. When omitted
  (and `skills` is also NULL), downloads pre-computed skills instead.

- decay_params:

  Decay parameters list. Only used when `match_stats` is provided.

- date:

  Date to estimate skills at (default today). Only used when
  `match_stats` is provided.

- min_weighted_90s:

  Regression threshold. Only used when `match_stats` is provided.

- skills:

  Pre-computed skills data.table from
  [`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)
  or
  [`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md).
  If provided, skips estimation and uses these directly.

- source:

  Data source for auto-loading: "remote" (default) or "local". Only used
  when both `match_stats` and `skills` are NULL.

## Value

A data.table with columns: category, stat, type, skill, raw_avg,
league_avg, league_pct, pos_avg, pos_pct, n90, w90, attempts,
w_attempts.

## Details

When called without `match_stats` or `skills`, automatically loads
pre-computed skill estimates via
[`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md)
(~2-3 MB download). This covers all 15 Opta leagues across all available
seasons.
