# xG override for shots direct from a corner kick

Applied in
[`add_xg_to_spadl()`](https://peteowen1.github.io/panna/reference/add_xg_to_spadl.md)
to shots `.is_direct_corner()` flags (Opta qualifier 263, or a
Corner-situation shot inside the corner-flag box). Opta logs a corner as
a shot only when it threatens the goal, so the shot table alone says 238
goals from 272 attempts (0.875), and the model learned that. Across all
1,253,311 corners taken in the v5.1 training matches the rate is
0.00019. Set to 0.02 (Pete, 2026-10-06, panna#277) so a logged attempt
is still priced as a real, if poor, chance rather than zero.

## Usage

``` r
DIRECT_CORNER_XG
```

## Format

Numeric value: 0.02
