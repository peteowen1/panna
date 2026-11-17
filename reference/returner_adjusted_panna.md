# Returning-player adjustment to Panna

Players coming back after a spell with no minutes deliver less than
their Panna says for their first few games back (panna#497). Across
8,404 returns after 60-179 days out, their team's xG difference with
them on the pitch fell 0.032 per 90 short of their Panna relative to
same-rated regulars. A shrink that grows with time away over-corrected
players back from 180+ days. A flat discount, tuned on returns before
2024-07-01, did best on later ones: keep 85% of the player's lead over
replacement level. Test loss fell from 0.0022 to 0.0009, and regulars
were unchanged
(`pannaverse/docs/reviews/panna-validity-2026-11/shrink_shapes_holdout.R`).

## Usage

``` r
returner_adjusted_panna(panna, replacement, returning, keep = 0.85)
```

## Arguments

- panna:

  Numeric vector of Panna ratings.

- replacement:

  Replacement-level Panna, length 1 or the same length as `panna`.

- returning:

  Logical vector, the same length as `panna`: is the player returning?
  See
  [`returner_status()`](https://peteowen1.github.io/panna/reference/returner_status.md).

- keep:

  Share of the lead over replacement kept while returning.

## Value

`panna` with returning players pulled towards `replacement`. `NA` inputs
stay `NA`.

## Details

The shortfall reads as a return dip (fitness) rather than an out-of-date
rating, so this is applied where a current-form number is wanted (the
team page, match predictions). It is never applied to the career rating
itself.

## Examples

``` r
returner_adjusted_panna(c(0.12, 0.12), -0.11, c(TRUE, FALSE))
#> [1] 0.0855 0.1200
```
