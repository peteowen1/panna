# Apply the PSV position calibration

Multiplies `psv` (and `osv`/`dsv` when present, so `osv + dsv == psv` is
preserved) by that player's position factor.

## Usage

``` r
apply_psv_calibration(
  psv_dt,
  position_col = "pos_grp",
  calibration = load_psv_calibration()
)
```

## Arguments

- psv_dt:

  A data.frame/data.table with `psv` and a position column.

- position_col:

  Name of the resolved-position column. Default `"pos_grp"`;
  `"primary_position"` and `"position"` are used as fallbacks when
  present.

- calibration:

  Calibration table; defaults to
  [`load_psv_calibration`](https://peteowen1.github.io/panna/reference/load_psv_calibration.md).

## Value

`psv_dt` with `psv` (and `osv`/`dsv`) scaled, and an attribute
`panna_psv_calibrated = TRUE`.

## Details

**Apply this BEFORE the league offsets, not after.** This LOOKS like it
contradicts the project's general calibration-ordering rule (see
`docs/reference/RATING_CALIBRATION.md`: PSR calibrates *after* its
additive league offset, because scaling first was measured to leave the
offset itself uncalibrated). PSV's case is different in a way that flips
the correct order: PSR's offset is a separately-estimated additive
constant that does not depend on PSR's own scale, so PSR can be scaled
either side of it. PSV's league offsets are not separate – they are
[`compute_psr_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md)
-\> `build_league_network(value_col = "psv")`, i.e. *derived from PSV
itself*. Calibrating after that derivation would leave the offsets
estimated on uncalibrated input, which is the one ordering the general
rule exists to rule out. In step 06 the sequence is: strip
`psv_league_offset` -\> `apply_psv_calibration()` -\>
[`compute_psr_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md).
Do not iterate: feeding offset-adjusted PSV back into the network makes
each cycle estimate the residual of its own previous output.

**The position column must be a RESOLVED position, not the per-match
role.** Opta records `position == "Substitute"` for anyone appearing off
the bench – roughly 29% of game-log rows as measured 2026-09 (an
absolute count would already be stale given the daily Opta scrape;
re-measure rather than trust a cached number). Bucketing on that field
puts a blend of every position into one group and biases the outfield
factors. Resolve each player's modal non-Substitute position first (per
season, career fallback); the factors shipped here were derived that
way.

Factors come from a **leak-free** fit – season S-1 PSV predicting season
S match outcomes, 450-minute prior gate. Fitting them against the same
match's goal difference is tautological for PSV (a keeper's PSV is built
from that match's saves and goals conceded) and inverts them.

**The shipped factors are SCALE-PRESERVING**: the raw fitted slopes
(`slope_raw`: GK 0.7847, DEF 1.3627, MID 1.5804, FWD 1.9319) are divided
by their minute-weighted mean (1.4833) so the overall PSV level is
unchanged and only the relative position weighting moves. That matters
because these offsets are ADDED to PSR at full strength on the
assumption that PSV and PSR share units. Using the raw slopes inflates
all of PSV ~1.5x and every league offset with it – which looks like a
large correction (+55% on weak leagues) but is a units artefact. The
genuine differential effect on the offsets is about **+4.6%**; the real
payoff of this table is to PSV itself, where it cuts goalkeeper share of
the top 1% from 4.2x the population rate to ~0.2x.

**These figures are a point-in-time measurement (2026-09), not an
enforced invariant** – there is no checked-in script that regenerates
`psv_calibration.csv` yet (unlike `07d_derive_psv_gd_scale.R` for
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md)).
If the PSR/PSV coefficients are ever retrained, re-derive these factors
rather than assume they still hold.

## See also

[`load_psv_calibration`](https://peteowen1.github.io/panna/reference/load_psv_calibration.md),
[`compute_psr_league_offsets`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md)

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`apply_psr_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_calibration.md),
[`apply_psr_season_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_season_calibration.md),
[`apply_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/apply_psv_opponent_adjustment.md),
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`fit_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/fit_psv_opponent_adjustment.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psr_calibration()`](https://peteowen1.github.io/panna/reference/load_psr_calibration.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`psr_leaderboard_eligible()`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md),
[`psr_season_coverage()`](https://peteowen1.github.io/panna/reference/psr_season_coverage.md),
[`psr_season_covered()`](https://peteowen1.github.io/panna/reference/psr_season_covered.md),
[`psv_opponent`](https://peteowen1.github.io/panna/reference/psv_opponent.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
