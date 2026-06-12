# Report Where Local Opta Data Is Coming From

Prints a diagnostic summary of how panna will resolve local data
lookups: which directory
[`pannadata_dir()`](https://peteowen1.github.io/panna/reference/pannadata_dir.md)
and
[`opta_data_dir()`](https://peteowen1.github.io/panna/reference/opta_data_dir.md)
point at, which consolidated parquet files exist, how many distinct
`(competition, season)` pairs each contains, which per-season
directories exist, and where the consolidated parquets disagree with the
per-season files.

## Usage

``` r
data_location_report(leagues = NULL)
```

## Arguments

- leagues:

  Optional character vector of Opta league codes (e.g.
  `c("World_Cup", "UEFA_Euros")`) to spot-check. Default `NULL`
  summarises all leagues found.

## Value

Invisibly returns a list of the gathered facts so the report can also be
consumed programmatically. The function's main value is the printed
output.

## Details

Use this whenever `load_opta_*()` returns nothing for data you believe
is on disk, or to confirm that a sync landed in the place panna actually
looks. The freshness skew between the consolidated parquet and
per-season files is the most common silent failure – this surfaces it.
