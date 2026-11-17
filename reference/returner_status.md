# Is a player returning from a spell with no minutes?

A player is returning on `as_of` when either:

- he is still out: no minutes for at least `gap_days` before `as_of`,
  while his last club played at least `club_matches` matches since; or

- he is back: his last such gap ended before `as_of`, and he has played
  fewer than `window_minutes` minutes since it ended. The gap counts
  only if the club he came back to had played at least `club_matches`
  matches in it and he had played for that club before the gap (so a
  transfer debut is not a return).

This is the same return rule as the panna#497 validity tests
(`respell()` / `build_hist()` in pannaverse
`docs/reviews/panna-validity-2026-11/panna_validity_common.R`), which is
what the 0.85 in
[`returner_adjusted_panna()`](https://peteowen1.github.io/panna/reference/returner_adjusted_panna.md)
was tuned on. Minutes count every competition in `lineups`, one row per
(match, player), capped at `minutes_cap` (the lineups hold a few corrupt
values in the tens of thousands). A player who retired or left the
covered competitions stays "still out" indefinitely (19.7% of the
100,378 players seen since 2024, as at 2026-11-09), so filter to the
players you are showing or predicting for before reading the share.

## Usage

``` r
returner_status(
  lineups,
  as_of,
  gap_days = 60L,
  club_matches = 8L,
  window_minutes = 900,
  minutes_cap = 120
)
```

## Arguments

- lineups:

  data.frame with `player_id`, `team_id`, `match_id`, `match_date` (Date
  or ISO string) and `minutes_played`. Rows on or after `as_of` are
  ignored, so the result is as-at.

- as_of:

  Date the status is wanted for.

- gap_days, club_matches, window_minutes:

  The return rule.

- minutes_cap:

  Minutes above this in one match are treated as this.

## Value

data.table with one row per player seen before `as_of`: `player_id`,
`returning` (logical), `days_out` (length of the current gap if he is
still out, else of his last qualifying gap; NA if none) and
`minutes_since_return` (0 while still out, even if he had an earlier
return; NA if he never had a qualifying gap).
