# Calculate EPR via weighted ridge regression with league/opponent FE

Per-(player, game) regression that simultaneously estimates per-player
skill (beta_player, returned as EPR) and league/opponent context
effects. Uses exponential time-decay weighting on observations.

## Usage

``` r
calculate_epr_regression(
  player_game_epv,
  ref_date = NULL,
  decay = 400,
  alpha = 0,
  lambda = NULL,
  prior_strength = 5,
  tier_interaction = TRUE,
  league_offsets = NULL,
  verbose = FALSE
)
```

## Arguments

- player_game_epv:

  Per-game EPV data. Required columns: `player_id`, `player_name`,
  `match_date`, `minutes_played`, `epv_offensive`, `epv_defensive`.
  Recommended additional columns: `league` (or `competition`),
  `season_end_year`, `opp_def_rating` (continuous opponent defensive
  strength, e.g., from RAPM-derived team ratings).

- ref_date:

  Snapshot date – only matches strictly before this are used.

- decay:

  Exponential decay constant in days for the time weight.

- alpha:

  glmnet mixing parameter (0 = pure ridge, recommended).

- lambda:

  Optional lambda. If NULL, uses the median of a 30-lambda path (good
  for ridge with sensible default shrinkage).

- prior_strength:

  Equivalent-games prior for shrinking small-sample players toward 0.
  Implemented by adding "phantom" zero-y rows weighted by
  `prior_strength` for each player. Set to 0 to disable.

- tier_interaction:

  If TRUE (default), fit player \* league-tier interaction – i.e. each
  player gets up to two beta coefficients, one for tier-1 (top-5 + UCL +
  WC/EURO) and one for tier-2 (everything else). This fixes cross-league
  standouts (Tavernier at Rangers, Veerman at PSV) whose single-beta was
  a compromise between their elite domestic per-90 and their modest
  UCL/UEL per-90. Set to FALSE for the legacy "one beta per player"
  behaviour. **Ignored when `league_offsets` is supplied**, since
  offsets supersede the coarse two-tier split with continuous per-league
  calibration.

- league_offsets:

  Optional data.table of per-league EPV calibration offsets from
  [`compute_league_offsets`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md).
  Must have columns `league`, `offset_off`, `offset_def`. When supplied,
  the per-row response is shifted to a UCL-equivalent scale via
  `y_off <- y_off + offset_off[league]` (and likewise for defence)
  before the regression runs, so `beta_player` is directly comparable
  across leagues. Leagues not present in the table are treated with a
  zero offset and a single warning is issued.

- verbose:

  If TRUE, print step timings.

## Value

A data.table with one row per player: `player_id`, `player_name`, `epr`,
`epr_offensive`, `epr_defensive`, `n_games`, `wt_games`.

## See also

Other epr:
[`PANNA_EPR_WEIGHT`](https://peteowen1.github.io/panna/reference/PANNA_EPR_WEIGHT.md),
[`PANNA_PSR_WEIGHT`](https://peteowen1.github.io/panna/reference/PANNA_PSR_WEIGHT.md),
[`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md),
[`build_player_game_ratings()`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md),
[`calculate_epr()`](https://peteowen1.github.io/panna/reference/calculate_epr.md),
[`calculate_epr_batch()`](https://peteowen1.github.io/panna/reference/calculate_epr_batch.md),
[`player_value()`](https://peteowen1.github.io/panna/reference/player_value.md)
