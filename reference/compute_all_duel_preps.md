# Build the per-contest feature tables for one league's events (memory-safe)

Orders the event stream ONCE and extracts all five (small) contest
tables, so a caller can loop leagues and discard raw events between
iterations.

## Usage

``` r
compute_all_duel_preps(events)
```

## Arguments

- events:

  Full per-league Opta events.

## Value

Named list of five finalized feature tables.

## See also

Other xduel:
[`compute_duel_woe()`](https://peteowen1.github.io/panna/reference/compute_duel_woe.md),
[`fit_duel_model()`](https://peteowen1.github.io/panna/reference/fit_duel_model.md),
[`load_duel_model()`](https://peteowen1.github.io/panna/reference/load_duel_model.md),
[`predict_duel()`](https://peteowen1.github.io/panna/reference/predict_duel.md),
[`prepare_duels_from_events()`](https://peteowen1.github.io/panna/reference/prepare_duels_from_events.md),
[`save_duel_model()`](https://peteowen1.github.io/panna/reference/save_duel_model.md)
