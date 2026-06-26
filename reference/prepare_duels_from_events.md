# Prepare one duel contest from raw (per-league) events

Prepare one duel contest from raw (per-league) events

## Usage

``` r
prepare_duels_from_events(events, contest = names(.DUEL_CONTESTS))
```

## Arguments

- events:

  Raw Opta events (`type_id`, `outcome`, `x`, `y`, `player_id`,
  `team_id`, `match_id`, `period_id`, `minute`/`second`). Pass the FULL
  event stream — `aerial_poss`/`containment` look at neighbouring rows
  of any type.

- contest:

  One of `aerial_win`, `aerial_poss`, `takeon`, `tackle_poss`,
  `containment`.

## Value

data.table of features + `won`, keyed columns retained.
