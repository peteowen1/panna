# Extract season end year from a season string

Handles both "YYYY-YYYY" format (returns second year) and tournament
"YYYY Country" format (returns the year).

## Usage

``` r
extract_season_end_year(season)
```

## Arguments

- season:

  Character vector of season strings (e.g., "2023-2024", "2018 Russia",
  "Intl_Friendlies_2024")

## Value

Numeric vector of end years, `NA_real_` where unparseable

## Details

VECTORIZED over `season`. It was scalar-only until 2026-08-17: the
opening guard was `if (is.na(season) || !nzchar(season))`, and `||` on a
length \> 1 argument is a hard error under R \>= 4.3. Every call site
had to remember to wrap it in
[`vapply()`](https://rdrr.io/r/base/lapply.html)/[`sapply()`](https://rdrr.io/r/base/lapply.html),
and one that didn't was a latent crash rather than a wrong number –
[`.season_end_year_for_date()`](https://peteowen1.github.io/panna/reference/dot-season_end_year_for_date.md)
(R/spm_asof.R) passed
[`extract_season_from_date()`](https://peteowen1.github.io/panna/reference/extract_season_from_date.md)
straight through. Scalar behaviour is unchanged, so existing
`vapply(x, extract_season_end_year, numeric(1))` call sites keep
working; new callers can pass the whole column.
