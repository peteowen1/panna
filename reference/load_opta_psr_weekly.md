# Load pre-computed weekly PSR snapshots

Downloads and queries `opta_psr_weekly.parquet` from the `opta-latest`
GitHub release. Contains PSR/OSR/DSR for every player at weekly (last 2
years) or monthly (older) snapshot dates.

## Usage

``` r
load_opta_psr_weekly(
  date = NULL,
  columns = NULL,
  source = c("remote", "local"),
  repo = "peteowen1/pannadata",
  tag = "opta-latest"
)
```

## Arguments

- date:

  Optional date filter. If provided, returns the snapshot for the
  nearest weekly date at or before this date. Accepts a `Date` or a
  character string parseable by
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html).

- columns:

  Optional character vector of columns to select.

- source:

  Data source: `"remote"` (default) or `"local"`.

- repo:

  GitHub repository (default: "peteowen1/pannadata").

- tag:

  Release tag (default: "opta-latest").

## Value

Data frame with columns: `snapshot_date`, `player_id`, `player_name`,
`primary_position`, `psr`, `osr`, `dsr`, `weighted_90s`.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Latest snapshot
psr <- load_opta_psr_weekly()

# Snapshot nearest to a specific date
psr <- load_opta_psr_weekly(date = "2026-03-18")
} # }
```
