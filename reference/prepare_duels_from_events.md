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

## See also

Other xduel:
[`compute_all_duel_preps()`](https://peteowen1.github.io/panna/reference/compute_all_duel_preps.md),
[`compute_duel_woe()`](https://peteowen1.github.io/panna/reference/compute_duel_woe.md),
[`fit_duel_model()`](https://peteowen1.github.io/panna/reference/fit_duel_model.md),
[`load_duel_model()`](https://peteowen1.github.io/panna/reference/load_duel_model.md),
[`predict_duel()`](https://peteowen1.github.io/panna/reference/predict_duel.md),
[`save_duel_model()`](https://peteowen1.github.io/panna/reference/save_duel_model.md)
