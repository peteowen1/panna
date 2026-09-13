# Position bucket for PSV calibration, pinned to the GK router

A thin wrapper over
[`resolve_position_group`](https://peteowen1.github.io/panna/reference/resolve_position_group.md)
– the canonical resolver, which already handles the hard part: Opta's
`position` is the match ROLE, so roughly 29\\ bucketing on that blends
every position into one group. It resolves each player's
minutes-weighted modal non-Substitute position per season, falls back to
their career modal, and only then to the row's own label. Do NOT
reimplement that here: a duplicated position classifier is exactly what
[`.psv_position_group`](https://peteowen1.github.io/panna/reference/dot-psv_position_group.md)'s
notes record going wrong before.

## Usage

``` r
.psv_pos_grp(dt, is_gk = .detect_gk_rows(dt))
```

## Arguments

- dt:

  Table with `position` and `player_id`; `total_minutes` and
  `season_end_year` improve the resolution when present.

- is_gk:

  Logical vector marking rows routed to the GK sub-model; defaults to
  the same detection the scorer uses.

## Value

Character vector of GK/DEF/MID/FWD, or `NA` where unresolved. `"GK"`
appears if and only if `is_gk` is `TRUE`.

## Details

What this adds is the one thing the resolver cannot know – which MODEL
scored the row.

## Why the bucket is pinned to the GK router

A calibration factor is only meaningful against the model it was fitted
on, so the bucket must describe the scoring path, not the player's true
position. `is_gk` therefore wins outright: a row the GK router sent to
the outfield model must never receive the GK factor.

UPDATE 2026-09-13:
[`.detect_gk_rows`](https://peteowen1.github.io/panna/reference/dot-detect_gk_rows.md)
was fixed to route the vast majority of these correctly (a \>50\\
player's other rows), so the case below is now rare rather than routine.
This pin function is kept as the safety net for what remains: a player
whose rows split exactly 50/50 defaults to non-GK, and any
caller-supplied `is_gk` that disagrees with the resolved position for
some other reason.

Historical case, now mostly resolved.
[`.detect_gk_rows`](https://peteowen1.github.io/panna/reference/dot-detect_gk_rows.md)
used to grep only the row's own `position`, which reads `"Substitute"`
for a keeper coming off the bench – so substitute keepers (measured
2026-09: 3,756 rows, 0.184\\ resolved position was GK. Without this pin
they would have taken the GK factor onto an outfield-model score. Any
row still like this returns `NA`, which
[`apply_psv_calibration`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md)
treats as factor 1 – honest, because that scoring path has no fitted
factor. Callers should report that count rather than let it pass
silently: it is a gap, not a known value.

The coefficient RETRAIN this routing fix calls for
(07_train_psr_model.R, whose GK/outfield training split also runs
through
[`.detect_gk_rows()`](https://peteowen1.github.io/panna/reference/dot-detect_gk_rows.md),
deliberately, so train and serve route identically) is a separate,
tracked step – the newly-captured GK rows are correctly SCORED now, but
the GK coefficients themselves were fit before those rows were part of
the GK training set.
