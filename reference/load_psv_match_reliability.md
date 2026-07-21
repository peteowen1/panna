# Load the bundled per-match reliability artifact for PSV pricing

Per-(model, stat) variance decomposition of the single-match per-90 rate
over players, built by `07b_build_position_means.R` over the same
enriched `match_stats` population as `position_role_means.csv`.
`lambda = Var_between / (Var_between + Var_within)` in `[0, 1]` is the
reliability of a single match as evidence of a persistent player level.
Pass the result as `reliability` to
[`compute_player_psv`](https://peteowen1.github.io/panna/reference/compute_player_psv.md)/[`calculate_psv_components`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md)/
[`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md)
to shrink each stat's contribution by its lambda (LIVE-PSV-UNBLOCK D1
v2; supersedes the v1 sd-swap design, which the empirical gate rejected
for amplifying stable-scale features).

## Usage

``` r
load_psv_match_reliability()
```

## Value

data.table(model, stat_name, n_players, n_player_matches, sd_match,
var_between, var_within, lambda), or NULL if the artifact is absent.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
