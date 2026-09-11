# Opponent adjustment for per-match PSV (panna#220)

PSV scores a player's box-score line for a single match. Nothing in that
line knows who the opposition was, so a defender facing a
bottom-of-the-table attack and one facing Manchester City are priced
identically. PSR gets an opponent control at training time
(`07_train_psr_model.R` passes the opponents' defensive ratings as
unpenalized regressors) and EPR gets one in its ridge
(`opp_def_rating`); PSV alone had none.

## Details

The adjustment is additive and deliberately minimal:

`psv_adj = psv - gamma * opp_def_rating`

`opp_def_rating` is the opposing team's season defensive strength from
`cache-opta/team_season_strength.parquet` – since 2026-09-03, in the
xRAPM sign convention where **positive is good defence**. `gamma` is
fitted empirically
([`fit_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/fit_psv_opponent_adjustment.md)),
so this formula is sign-convention-agnostic: whichever sign
`opp_def_rating` holds, the fitted `gamma` comes out with the sign that
makes `-gamma * opp_def_rating` raise the adjusted value for a strong
defence and lower it for a leaky one. **Never hardcode a fitted `gamma`
from one sign convention against data in the other** – re-fit whenever
`team_season_strength.parquet`'s `sign_convention` tag changes (see
`TEAM_STRENGTH_SIGN_CONVENTION`).

## Why a single global gamma

Per-position or per-league gammas were not fitted. The relationship
being estimated is "how much of a player's box output is explained by
the opposition", which has no strong prior reason to differ by position,
and the project's experience with splitting a calibration across two
axes at once is that the joint cells lose power long before the
marginals do (see `docs/reference/RATING_CALIBRATION.md` on the position
x season grid). Start with the marginal; split it only if a residual
check demands it.

## Not enabled by default

This changes PSV, and PSV is the input to
[`build_league_network()`](https://peteowen1.github.io/panna/reference/build_league_network.md),
which produces the league offsets, which are added to PSR. Turning it on
therefore moves three things at once, and a comparison arm that differs
on more than one axis measures nothing. It must be validated as its own
single-axis change against a re-run baseline, not bundled into another
retrain.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`apply_psv_calibration()`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md),
[`apply_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/apply_psv_opponent_adjustment.md),
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`fit_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/fit_psv_opponent_adjustment.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
