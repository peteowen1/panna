# Calculate EPR (Expected Possession Rating) from per-game EPV

For each player, applies exponential time-decay to their per-game EPV
values and computes a Bayesian-shrunk rating estimate. Players with
little data are shrunk toward the prior rate (slightly below zero for
offense, zero for defense).

## Usage

``` r
calculate_epr(
  player_game_epv,
  ref_date = NULL,
  decay_offensive = EPR_DECAY_OFFENSIVE,
  decay_defensive = EPR_DECAY_DEFENSIVE,
  prior_games = EPR_PRIOR_GAMES,
  prior_rate_off = EPR_PRIOR_RATE_OFF,
  prior_rate_def = EPR_PRIOR_RATE_DEF,
  loading = EPR_LOADING,
  league_baseline = TRUE
)
```

## Arguments

- player_game_epv:

  Per-game EPV data from
  [`aggregate_player_game_epv`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md).
  Must contain: `player_id`, `match_id`, `match_date`, `minutes_played`,
  `epv_offensive`, `epv_defensive`.

- ref_date:

  Date to estimate ratings at. Only matches before this date are used.
  If NULL, uses the latest match date in data.

- decay_offensive:

  Decay rate in days for offensive EPV (default `EPR_DECAY_OFFENSIVE`).

- decay_defensive:

  Decay rate in days for defensive EPV (default `EPR_DECAY_DEFENSIVE`).

- prior_games:

  Prior strength in equivalent full games (default `EPR_PRIOR_GAMES`).

- prior_rate_off:

  Prior rate for offensive component (default `EPR_PRIOR_RATE_OFF`).

- prior_rate_def:

  Prior rate for defensive component (default `EPR_PRIOR_RATE_DEF`).

- loading:

  Loading factor applied to observed data (default `EPR_LOADING`).

- league_baseline:

  Logical. If TRUE (default) and the input has a `league` column,
  per-(league, season) baseline EPV credit is subtracted from each row's
  per-90 credit before the decay-weighted aggregation. This makes EPR
  cross-league comparable: a player dominating in a weaker league is
  judged relative to that league's baseline rather than the global one.
  Set to FALSE to restore pre-baseline behaviour.

## Value

A data.table with one row per player:

- player_id, player_name:

  Identifiers

- epr:

  Total EPR = epr_offensive + epr_defensive

- epr_offensive:

  Offensive EPV rating (passing, shooting, dribbling)

- epr_defensive:

  Defensive EPV rating (defending, duel blame)

- wt_games:

  Weighted games (effective sample size)

- n_games:

  Raw number of games played
