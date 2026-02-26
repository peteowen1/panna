# Aggregate skills for SPM input (one row per player per season)

Produces a data frame in the same format as
[`aggregate_opta_stats()`](https://peteowen1.github.io/panna/reference/aggregate_opta_stats.md),
where each row is one player's estimated skill profile at the end of a
season. This is a drop-in replacement for SPM input.

## Usage

``` r
aggregate_skills_for_spm(
  match_stats,
  decay_params = NULL,
  season_end_dates = NULL,
  min_minutes = 450,
  min_weighted_90s = 5
)
```

## Arguments

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).

- decay_params:

  Decay parameters (from
  [`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md)
  or optimized).

- season_end_dates:

  Named list mapping season_end_year (integer) to a Date. Skills are
  estimated as of this date for each season. If NULL, uses June 30 of
  each year.

- min_minutes:

  Minimum total minutes in the season for inclusion.

- min_weighted_90s:

  Minimum weighted 90s for regression threshold.

## Value

A data.table with one row per player per season_end_year, containing
skill estimates for all stats plus identity/context columns. Compatible
with
[`fit_spm_opta()`](https://peteowen1.github.io/panna/reference/fit_spm_opta.md).
