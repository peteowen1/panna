# Compute per-league per-90 EPV offsets vs an anchor league

Estimates an additive offset for each league L of the form
`offset_L = mean(anchor_y - league_y)` on the per-90 EPV scale, where
`anchor` defaults to UCL group stage. Offsets are recency-weighted
(exponential decay, half-life `half_life` years).

## Usage

``` r
compute_league_offsets(
  game_logs,
  ref_year = NULL,
  half_life = 3,
  anchor_league = "UCL",
  exclude_qualifiers = TRUE,
  min_ucl_n = 4L,
  min_dom_n = 8L,
  min_ucl_mins = 180,
  min_dom_mins = 720,
  min_n_for_same_season = 20L,
  min_career_games = 10L,
  chain_intermediates = c("POR", "ESP", "ITA", "ENG", "FRA", "GER"),
  prefer_chained_for = c("BRA"),
  verbose = TRUE
)
```

## Arguments

- game_logs:

  Per-game EPV data. Required columns: `player_id`, `match_date`,
  `league`, `minutes_played` (or `total_minutes`), `epv_offensive`,
  `epv_defensive`. Typically the rbinded `game_logs_*.parquet` files in
  `data-raw/cache-predictions-opta/`.

- ref_year:

  Reference year used to compute recency weight. Defaults to
  `max(year(match_date)) + 1` (i.e., the upcoming season).

- half_life:

  Years for exponential decay of season weights (default 3).

- anchor_league:

  Anchor league string (default `"UCL"`). Offsets are reported relative
  to this league's group-stage games.

- exclude_qualifiers:

  If TRUE (default), drops UCL/UEL/UECL qualifier rounds (matches before
  September 15 of a season-end-year) which feature weaker teams and
  would inflate the anchor's apparent difficulty.

- min_ucl_n, min_dom_n:

  Minimum number of games in each league for a bridging player-season to
  count (defaults 4 and 8).

- min_ucl_mins, min_dom_mins:

  Minimum minutes in each league for a bridging player-season (defaults
  180 and 720).

- min_n_for_same_season:

  Minimum player-seasons needed before `"same-season"` is preferred over
  career-trajectory (default 20).

- min_career_games:

  Minimum games per league for a career-trajectory bridge (default 10).

- chain_intermediates:

  Candidate intermediate leagues for chaining leagues without direct
  anchor bridge.

- prefer_chained_for:

  Character vector of league codes for which the chained estimate should
  be used even when a direct career-trajectory bridge is available.
  Default `c("BRA")` because Brazilian-league bridges to UCL are
  dominated by career-stage bias (young-and-developing in Brazil, peak
  in Europe), so the direct estimate is unreliable.

- verbose:

  If TRUE (default), prints a per-league summary.

## Value

A data.table with one row per league plus an anchor row:

- league:

  League code.

- method:

  `"same-season"`, `"career-direct"`, `"chained"`, or `"anchor"`.

- anchor:

  Anchor league used.

- n_obs:

  Number of bridging player-seasons (same-season) or player-pairs
  (career/chained).

- offset_off, offset_def, offset_tot:

  Per-90 EPV offsets on the anchor-equivalent scale.

## Details

Three estimators are tried per league, in priority order:

1.  `"same-season"` – same player, same season, both leagues. Cleanest
    because it eliminates career-stage confounds.

2.  `"career-direct"` – same player across career, both leagues. Broader
    coverage; some bias from career-stage effects.

3.  `"chained"` – for leagues with no direct anchor bridge, chain via an
    intermediate league with both bridges.
