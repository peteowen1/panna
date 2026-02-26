# Detect and filter bad xG data from splints

Identifies league-seasons with high rates of missing/zero xG data and
filters them out. Reports what was filtered.

## Usage

``` r
filter_bad_xg_data(splint_data, zero_xg_threshold = 20, verbose = TRUE)
```

## Arguments

- splint_data:

  Splint data list from create_all_splints

- zero_xg_threshold:

  Percentage threshold for flagging bad data (default 20%)

- verbose:

  Whether to print filtering report

## Value

List with filtered splint_data and filtering report
