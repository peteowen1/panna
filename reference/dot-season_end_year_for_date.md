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

  Date (or coercible)

## Value

Integer season end year (e.g. 2026 for a date in season "2025-2026")
