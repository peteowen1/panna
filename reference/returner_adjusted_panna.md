# Returning-player adjustment to Panna

Players coming back from an absence deliver less than their Panna says
for their first games back (panna#497). The rule has no cliffs (Pete,
2026-11-17): each player carries a "rust" value between 0 and 1 (see
[`returner_rust()`](https://peteowen1.github.io/panna/reference/returner_rust.md)),
and his Panna keeps `1 - max_discount * rust` of its lead over
replacement level. With the defaults that is 0.875 after missing 4 club
matches and 0.75 after 8 or more, fading back to 1 over his first 1,800
minutes back. Chosen on a hold-out (returns and fringe players before
2024-07-01 to fit, later ones to score; eight groups, regulars held
still): test loss 0.00083 against 0.00075 for the earlier flat rule
(difference -0.00024 to +0.00041) and 0.00160 for none
(`pannaverse/docs/reviews/panna-validity-2026-11/shrink_fringe_holdout.R`,
`shrink_bench_holdout.R`, `shrink_bench_w.R`). An unused-substitute game
counts 0.75 of a missed match (Pete, 2026-11-18; the data could not
separate bench from not-in-squad, train loss within 1% for any weight
0-1).

## Usage

``` r
returner_adjusted_panna(panna, replacement, rust, max_discount = 0.25)
```

## Arguments

- panna:

  Numeric vector of Panna ratings.

- replacement:

  Replacement-level Panna, length 1 or the same length as `panna`.

- rust:

  Numeric vector in \[0, 1\], the same length as `panna`; NA counts as
  0.

- max_discount:

  Share of the lead over replacement removed at rust 1.

## Value

`panna` pulled towards `replacement` by `max_discount * rust`. `NA`
ratings stay `NA`.

## Details

The shortfall reads as a return dip (fitness) rather than an out-of-date
rating, so this is for a current-form number (the team page). It is
never applied to the career rating itself, and match predictions do not
use it (out-of-sample error unchanged, 1.0915 -\> 1.0914 on 675
matches).

## Examples

``` r
returner_adjusted_panna(c(0.12, 0.12), -0.11, c(1, 0))
#> [1] 0.0625 0.1200
```
