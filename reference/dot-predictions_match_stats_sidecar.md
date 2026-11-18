# Path of a 01_match_stats side file, or NULL when it is missing or older than the table

A side file written before the full table was last rebuilt describes old
data, so it is ignored and the caller reads the full table.

## Usage

``` r
.predictions_match_stats_sidecar(
  match_stats_path,
  which = c("narrow", "recent")
)
```
