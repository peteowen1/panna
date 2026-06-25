# Estimate per-league strength from the full same-season co-occurrence network

A connected, all-bridges alternative to
[`compute_league_offsets`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md).
Instead of bridging every league only to a single anchor (UCL), it uses
**every** same-season pairing a player straddles — domestic league,
continental cup (UCL/UEL), and international tournament (WC/Euro/Copa) —
and solves the whole graph at once via a player-season fixed effect.

## Usage

``` r
build_league_network(
  game_logs,
  value_col = "psv",
  big5 = c("ENG", "ESP", "GER", "ITA", "FRA"),
  min_mins = 270,
  shrink_k = 3,
  verbose = TRUE
)
```

## Arguments

- game_logs:

  Per-game data.frame/data.table with `player_id`, `league`,
  `total_minutes`, the `value_col`, and either `season`
  ("YYYY-YYYY"/"YYYY") or `season_end_year`.

- value_col:

  Per-game value column to league-adjust (e.g. `"psv"`, `"epv_total"`).
  Default `"psv"`.

- big5:

  League codes whose mean is the zero anchor. Default the five major
  European leagues (game-log 3-letter codes).

- min_mins:

  Minimum minutes in a (player, season, league) cell for it to count as
  a bridge endpoint. Default 270 (≈3 full games).

- shrink_k:

  Small-sample shrinkage: each league's offset is multiplied by
  `n_bridge / (n_bridge + shrink_k)` so a thin-N league can't
  over-swing. Default 3 (gentle — only meaningfully damps n \< ~10). Set
  0 to disable.

- verbose:

  Print the table. Default TRUE.

## Value

A data.table: `league`, `strength` (raw league-ease coefficient),
`offset` (`= -strength * shrink`, add to value to neutralize),
`n_bridge` (multi-league player-seasons touching it).

## Details

Concretely it regresses each player-season-league per-90 `value_col` on
league dummies plus a player-season fixed effect (implemented as a
within-player-season demeaning). The fixed effect absorbs each player's
overall level, so the league coefficient is the *within-player-season*
league effect — i.e. how much more value a player generates in that
league than in their other competitions that season ("league ease"). The
returned `offset` is the negation (the amount to ADD to a player's value
to put them on a league-neutral, Big-5-equivalent scale).

Versus `compute_league_offsets` this (a) multiplies the effective bridge
count (UCL goes from a lone anchor to thousands of bridges), (b)
connects isolated leagues through the international web (an A-League
player who played the World Cup links A-League into the graph without
ever touching UCL), and (c) is metric-agnostic — run it on `psv` to
league-adjust PSR, on `epv_total` for EPR, etc. Each metric gets its own
table on its own scale.

## See also

[`compute_league_offsets`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md),
[`compute_psr_league_offsets`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md)
