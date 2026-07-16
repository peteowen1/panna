# Apply cross-league EPV offsets to an EPR table (end-add, PSR-consistent)

Adds the per-league network offset to each row's `epr_offensive` /
`epr_defensive` (and recomputes `epr = epr_offensive + epr_defensive`),
placing league-season-centred EPR on a single Big-5-equivalent scale.
This is the EPR analogue of
[`apply_psr_league_offsets`](https://peteowen1.github.io/panna/reference/apply_psr_league_offsets.md):
run
[`calculate_epr_regression`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md)
with the league-season FE kept and `league_offsets = NULL` (so
\\\beta\_{player}\\ is "above league-season mean"), then add the offset
HERE.

## Usage

``` r
apply_epr_league_offsets(epr_dt, offsets, verbose = FALSE)
```

## Arguments

- epr_dt:

  A data.table/data.frame with a `league` column and
  `epr_offensive`/`epr_defensive` (and optionally `epr`).

- offsets:

  Offset table with columns `league`, `offset_off`, `offset_def` (e.g.
  from
  [`build_league_network()`](https://peteowen1.github.io/panna/reference/build_league_network.md)
  on offensive and defensive EPV).

- verbose:

  Report how many rows were adjusted. Default FALSE.

## Value

`epr_dt` (as data.table) with `epr_offensive`/ `epr_defensive`/`epr`
shifted, plus an `epr_league_offset` column recording the applied total.
Rows whose league has no offset are unchanged (offset 0).

## Details

Why end-add rather than shifting \\y\\ inside the regression: the offset
is a league-LEVEL quantity (estimated from the whole co-occurrence
network), so it should be applied at full strength, not discounted by
each player's ridge shrinkage. Shifting \\y\\ and shrinking \\\beta\\
toward 0 pulls low-sample weak-league players back toward the GLOBAL
mean — the opposite of the intent. Keeping the FE then end-adding
shrinks each player toward their own LEAGUE prior, which is the correct
behaviour (and matches PSR + unlocks the additive fast-path for
offset-only changes).

## See also

[`apply_psr_league_offsets`](https://peteowen1.github.io/panna/reference/apply_psr_league_offsets.md),
[`build_league_network`](https://peteowen1.github.io/panna/reference/build_league_network.md)

Other league offsets:
[`apply_psr_league_offsets()`](https://peteowen1.github.io/panna/reference/apply_psr_league_offsets.md),
[`build_league_network()`](https://peteowen1.github.io/panna/reference/build_league_network.md),
[`compute_psr_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md)
