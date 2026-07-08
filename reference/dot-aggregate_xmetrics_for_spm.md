# Aggregate an xMetrics table to player-level per-90 SPM features

THE ONE implementation of "given an xmetrics table (any subset — full
history or a single season), compute player-level per-90 above-expected
features for SPM enrichment." Extracted 2026-07-08 (panna#87) after this
exact logic was independently duplicated in `05_spm.R` (all-history SPM
fit) and `07_seasonal_ratings.R` (per-season SPM breakdown) — the second
script's copy never got the xDuel WOE / finishing over-performance
columns added to the first, so a season-level
[`calculate_spm_ratings()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md)
call errored with "undefined columns selected" the moment the fitted
model's `predictor_cols` included any of them (every one of 14 seasons
failed identically on the first cloud run after the SPM modernization
shipped). One implementation closes the class of bug, not just this
instance.

## Usage

``` r
.aggregate_xmetrics_for_spm(xmetrics)
```

## Arguments

- xmetrics:

  Data frame with (at least) `player_id`, `minutes`, `xg`, `npxg`, `xa`,
  `xpass_overperformance`, plus whichever above-expected columns this
  vintage carries (schema-defensive — the five xDuel WOE columns,
  finishing over-performance, placement, gsaa; see
  [`.spm_xmetrics_per90_cols()`](https://peteowen1.github.io/panna/reference/dot-spm_xmetrics_per90_cols.md)).

## Value

Data frame keyed by `player_id` with whichever of
[`.spm_xmetrics_per90_cols()`](https://peteowen1.github.io/panna/reference/dot-spm_xmetrics_per90_cols.md)
are derivable from `xmetrics` (fewer columns for an older vintage or a
thin season/subset — the caller ensures the full canonical set exists
before modeling).
