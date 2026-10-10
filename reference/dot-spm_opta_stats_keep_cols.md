# Columns of the Opta stats and xmetrics tables the SPM steps read

Steps 05 (SPM fit) and 07 (seasonal ratings) read only these columns of
the ~289-column player-match stats table and the ~69-column xmetrics
table. Step 02 writes a narrowed copy (`02_opta_stats_narrow.rds`) so
those steps never deserialize the full ~9.5GB stats table, which put
step 05 at 15.6-15.8GB of the 16GB runner and killed it on 2026-10-10
(panna#87). Readers of the stats table:
[`aggregate_opta_stats()`](https://peteowen1.github.io/panna/reference/aggregate_opta_stats.md)
(every column in
[`.get_opta_col_mapping()`](https://peteowen1.github.io/panna/reference/dot-get_opta_col_mapping.md),
plus `match_id`, `player_name`, `position`),
[`.ensure_player_id()`](https://peteowen1.github.io/panna/reference/dot-ensure_player_id.md),
[`.spm_league_shares()`](https://peteowen1.github.io/panna/reference/dot-spm_league_shares.md)
(a competition column and a minutes column), and step 07's season
filters (`season`). Readers of xmetrics:
[`.aggregate_xmetrics_for_spm()`](https://peteowen1.github.io/panna/reference/dot-aggregate_xmetrics_for_spm.md)
and the chain-feature blocks in 05/07.
`tests/testthat/test-spm-opta-helpers.R` checks that each reader gives
identical output on the narrowed table.

## Usage

``` r
.spm_opta_stats_keep_cols(nm)

.spm_xmetrics_keep_cols(nm)
```

## Arguments

- nm:

  Column names of the table being narrowed.

## Value

The subset of `nm` to keep, in `nm`'s order.
