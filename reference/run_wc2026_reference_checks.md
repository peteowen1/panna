# Run the WC2026 Reference Facts Against Pipeline Outputs

Loads the canonical output files (wc2026_team_strength.parquet,
wc2026_simulation.parquet, 04_match_dataset.rds), runs every
`WC2026_REFERENCE_FACTS` entry whose `check_*` field matches an
available data source, and emits one consolidated cli warning naming the
failures + the human-readable fact that motivated each check.

## Usage

``` r
run_wc2026_reference_checks(
  cache_dir = file.path("data-raw", "cache-predictions-opta")
)
```

## Arguments

- cache_dir:

  Directory holding the pipeline output files. Defaults to the
  pipeline's standard location.

## Value

Invisibly: the named list of failed checks (one element per failed fact,
value = the `fact` string).

## Details

Designed to be called at the end of the predictions pipeline (after step
12). Returns the number of failed checks invisibly.

## See also

Other world cup simulation:
[`bt_match_prob()`](https://peteowen1.github.io/panna/reference/bt_match_prob.md),
[`build_knockout_lookup()`](https://peteowen1.github.io/panna/reference/build_knockout_lookup.md),
[`compute_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md),
[`fit_bt_ratings()`](https://peteowen1.github.io/panna/reference/fit_bt_ratings.md),
[`match_is_international()`](https://peteowen1.github.io/panna/reference/match_is_international.md),
[`mirror_match_rows()`](https://peteowen1.github.io/panna/reference/mirror_match_rows.md),
[`simulate_world_cup()`](https://peteowen1.github.io/panna/reference/simulate_world_cup.md)
