# Aggregate player statistics to per-90 rates

Combines match-level statistics into per-90-minute rates for each
player. Extracts comprehensive features from all available stat tables
for SPM modeling. Includes derived features like success rates and
ratios.

## Usage

``` r
aggregate_player_stats(
  stats_summary,
  stats_passing = NULL,
  stats_defense = NULL,
  stats_possession = NULL,
  stats_misc = NULL,
  stats_passing_types = NULL,
  stats_keeper = NULL,
  min_minutes = 450
)
```

## Arguments

- stats_summary:

  Summary stats data frame from process_all_data

- stats_passing:

  Passing stats data frame (optional)

- stats_defense:

  Defense stats data frame (optional)

- stats_possession:

  Possession stats data frame (optional)

- stats_misc:

  Miscellaneous stats data frame (optional) - fouls, aerials, recoveries

- stats_passing_types:

  Passing types data frame (optional) - through balls, switches

- stats_keeper:

  Goalkeeper stats data frame (optional) - saves, post-shot xG

- min_minutes:

  Minimum total minutes for inclusion

## Value

Data frame with per-90 rates for each player
