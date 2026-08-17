# Season end year for a reference/match date

Composes the two existing season helpers
([`extract_season_from_date()`](https://peteowen1.github.io/panna/reference/extract_season_from_date.md) +
[`extract_season_end_year()`](https://peteowen1.github.io/panna/reference/extract_season_end_year.md))
so as-of-date consumers can map a Date straight to the `season_end_year`
grain the expanding-window SPM models (above) are keyed by, instead of
re-deriving the Aug-July season boundary.

## Usage

``` r
.season_end_year_for_date(date)
```

## Arguments

- date:

  Date vector (or coercible)

## Value

Numeric vector of season end years (e.g. 2026 for a date in season
"2025-2026")

## Details

Vectorized over `date` since
[`extract_season_end_year()`](https://peteowen1.github.io/panna/reference/extract_season_end_year.md)
was vectorized (2026-08-17). Before that this composition was a latent
crash on any vector input:
[`extract_season_from_date()`](https://peteowen1.github.io/panna/reference/extract_season_from_date.md)
is vectorized, so it handed a length-n character vector to a scalar-only
helper whose `||` guard errors under R \>= 4.3.
