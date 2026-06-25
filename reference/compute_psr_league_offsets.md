# Estimate cross-league PSR offsets from the per-game PSV network

PSR is computed from box-score skill rates, which vary little across
leagues, so a strong player in a weak league posts an inflated PSR. This
computes a per-league additive offset to neutralize that, using PSR's
own per-game analogue — **PSV** — through the same-season co-occurrence
network
([`build_league_network`](https://peteowen1.github.io/panna/reference/build_league_network.md)):
every same-season pairing a player straddles (domestic + continental +
international) is solved jointly via a player-season fixed effect.
Because PSV is PSR's own per-game value, the resulting offset is on the
right scale and is applied to PSR directly (no cross-metric rescaling).
Each metric league-adjusts with its own signal — EPR uses the EPV
network, PSR uses the PSV network, Panna needs none (RAPM already
controls opponents).

## Usage

``` r
compute_psr_league_offsets(
  game_logs,
  big5 = c("ENG", "ESP", "GER", "ITA", "FRA"),
  shrink_k = 3,
  verbose = FALSE
)
```

## Arguments

- game_logs:

  Per-game data with `player_id`, `league`, `total_minutes`, `psv`, and
  `season`/`season_end_year` (the rbinded `game_logs_*.parquet` files).

- big5:

  Big-5 anchor league codes (game-log 3-letter form). Default the five
  majors.

- shrink_k:

  Small-N shrinkage passed to `build_league_network` (default 3 —
  gentle).

- verbose:

  Print the offset table. Default TRUE.

## Value

A data.table with columns `league` (display competition name), `offset`
(add to PSR), and `n_bridge`.

## Details

Game-log league codes (e.g. `ENG`, `AUS`) are mapped to the displayed
competition names (`EPL`, `A_League`) via
[`to_opta_league`](https://peteowen1.github.io/panna/reference/to_opta_league.md)
so the result joins straight onto the seasonal PSR table.

## See also

[`build_league_network`](https://peteowen1.github.io/panna/reference/build_league_network.md),
[`apply_psr_league_offsets`](https://peteowen1.github.io/panna/reference/apply_psr_league_offsets.md)
