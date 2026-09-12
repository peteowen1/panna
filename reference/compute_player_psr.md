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
  target = c("blend", "xg", "goals"),
  position_means = NULL,
  gk_goal_scale = 1
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

  Outfield coefficient set: `"blend"` (default,
  `0.6 * xG + 0.4 * goals`), `"xg"`, or `"goals"`. The GK sub-model
  always uses goal differential regardless.

  Default moved `"xg"` -\> `"blend"` (panna#214). The xG-trained set
  cannot reward finishing by construction — `npg_minus_npxg_per90` is
  precisely the part of scoring xG does not capture — which made the net
  effect of an extra non-penalty goal NEGATIVE (-0.059) and left Messi
  93rd among outfielders. Measured out-of-sample (prior-season rating
  -\> next season's matches, fit \<=2022 / test \>=2023, n = 15,912),
  blend is the only set never worse than second: best on xG difference
  (test R2 0.1664 vs 0.1618 xG / 0.1611 goals) and a statistical tie for
  best on goal difference (0.1428 vs 0.1431 goals / 0.1360 xG).
  Differences across sets are small (\<1 R2 point), so the switch costs
  no predictive accuracy while materially improving valuation.

- position_means:

  Optional pre-computed position-mean lookup table used to center skill
  columns before scoring (see
  [`compute_player_psv`](https://peteowen1.github.io/panna/reference/compute_player_psv.md)).
  If `NULL`, no cross-position centering is applied.

- gk_goal_scale:

  **Superseded** by
  [`apply_psr_calibration`](https://peteowen1.github.io/panna/reference/apply_psr_calibration.md),
  which covers goalkeepers along with every other position AND is
  applied at the correct point (after the cross-league offsets).
  Defaults to `1` (no-op). Retained only so a caller can pin the old
  panna#202 behaviour by passing `GK_PSR_GOAL_SCALE` and skipping
  [`apply_psr_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_calibration.md);
  doing both would double-scale keepers.

## Value

A data.table with `psr`, `osr`, `dsr` columns.

## Details

GKs and outfield players are centered separately within their respective
populations, then combined.
