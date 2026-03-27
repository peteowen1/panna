# Compute PSR from skills using bundled coefficients

Convenience wrapper that loads pre-trained coefficients and computes PSR
with OSR/DSR decomposition (if offensive/defensive coefficient files are
available).

## Usage

``` r
compute_player_psr(skills, center = TRUE, target = c("xg", "goals"))
```

## Arguments

- skills:

  Player skill data (output of
  [`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)
  or
  [`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md)).

- center:

  Logical. Center PSR around league mean (default TRUE).

- target:

  One of `"xg"` (default) or `"goals"`.

## Value

A data.table with `psr`, `osr`, `dsr` columns.
