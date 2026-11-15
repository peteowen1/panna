# Score extra-time matches from the events, not the 90-minute fixture score

`opta_fixtures.parquet` stores the 90-minute score: Dinamo Zagreb v Thun
(2026-07-28) reads 2-2 there, 3-2 in the events after a period-4 goal.
Net goals reconciles each team to its goal difference, so without this
the value of an extra-time goal is reconciled away from the side that
scored it (panna#463: 703 of the 704 matches with an extra-time goal
differ).

## Usage

``` r
.ng_fixtures_extra_time(fixtures, events, lineups)
```

## Arguments

- fixtures:

  Fixtures with `match_id`, `home_team_id`, `away_team_id`,
  `home_score`, `away_score`.

- events:

  Opta events with `match_id`, `period_id`, `type_id`, `team_id` and
  `qualifier_json` (own goals).

- lineups:

  Opta lineups (home/away side per match).

## Value

`fixtures` as a data.table with the scores replaced on extra-time
matches, and attributes `n_extra_time` (matches rescored), `n_changed`
(scores that moved) and `n_unmatched` (teams did not match).

## Details

Only matches with an event in period 3 or 4 are rescored, from
[`.build_match_results_from_events()`](https://peteowen1.github.io/panna/reference/dot-build_match_results_from_events.md)
(extra time in, shootout out). A normal match keeps its fixture score,
so a thin feed missing a goal event cannot move its target. The events'
home/away sides are matched to the fixture's by team id; a match whose
two teams do not match is left as it is and counted.
