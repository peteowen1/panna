# How much "rust" a player carries from time out

Rust rises with the club matches a player missed and drains as he plays:

- at each appearance, the club matches his previous club played between
  his last appearance and this one, with each match he sat on the bench
  unused counting `bench_weight` and each he was not in the squad
  counting 1, `m`, give `s(m) = (m / centre)^k / (1 + (m / centre)^k)`,
  an S-curve (0 for no missed matches, 0.5 at `centre`, close to 1 well
  past it). Counting club matches, not days, so the summer break adds
  nothing;

- rust before the match is `max(s(m), rust carried)`;

- after the match it drops by `minutes / drain_minutes`, never below 0.

On `as_of`, a player still out carries
`max(s(matches missed so far), rust carried)`. Minutes count every
competition in `lineups`, one row per (match, player), capped at
`minutes_cap` (the lineups hold a few corrupt values in the tens of
thousands). A player who retired or left the covered competitions keeps
accruing missed club matches, so filter to the players you are showing
before reading any share.

## Usage

``` r
returner_rust(
  lineups,
  as_of,
  centre = 4,
  k = 6,
  drain_minutes = 1800,
  bench_weight = 0.75,
  minutes_cap = 120
)
```

## Arguments

- lineups:

  data.frame with `player_id`, `team_id`, `match_id`, `match_date` (Date
  or ISO string) and `minutes_played`. Rows on or after `as_of` are
  ignored, so the result is as-at.

- as_of:

  Date the rust is wanted for.

- centre, k:

  S-curve centre (club matches missed) and steepness.

- drain_minutes:

  Minutes back after which the rust from one absence is gone.

- bench_weight:

  How much an unused-substitute game counts, against 1 for not in the
  squad.

- minutes_cap:

  Minutes above this in one match are treated as this.

## Value

data.table, one row per player seen before `as_of`: `player_id`, `rust`,
and for the absence behind it `club_matches_missed`, `days_out` and
`minutes_since_return` (0 while still out; all three NA when rust is 0).
