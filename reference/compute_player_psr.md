# Compute PSR from skills using bundled coefficients

Convenience wrapper that loads pre-trained coefficients and computes PSR
with OSR/DSR decomposition. Automatically routes goalkeepers through a
separate GK sub-model (trained on goal differential with GK-specific
features) and outfield players through the standard xG-based model.

## Usage

``` r
compute_player_psr(
  skills,
  center = TRUE,
  target = c("xg", "goals"),
  position_means = NULL
)
```

## Arguments

- skills:

  Player skill data (output of
  [`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)
  or
  [`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md)).

- center:

  Logical. Center PSR around position-group mean (default TRUE).

- target:

  One of `"xg"` (default) or `"goals"` for the outfield model. GK model
  always uses goal differential.

## Value

A data.table with `psr`, `osr`, `dsr` columns.

## Details

GKs and outfield players are centered separately within their respective
populations, then combined.
