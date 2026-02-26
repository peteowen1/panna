# Aggregate Opta player statistics to per-90 rates

Combines match-level Opta statistics into per-90-minute rates for each
player. Creates comprehensive features for SPM modeling from Opta's 263
columns.

## Usage

``` r
aggregate_opta_stats(opta_stats, min_minutes = 450)
```

## Arguments

- opta_stats:

  Data frame from load_opta_stats()

- min_minutes:

  Minimum total minutes for inclusion (default 450)

## Value

Data frame with per-90 rates for each player

## Examples

``` r
if (FALSE) { # \dontrun{
opta_stats <- load_opta_stats("ENG", "2024-2025")
player_features <- aggregate_opta_stats(opta_stats, min_minutes = 450)
} # }
```
