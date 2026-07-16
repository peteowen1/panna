# Aggregate duels-above-expected per player (optionally per match)

Above-expected analogue of the old accuracy ratios: for each of the five
contests, contests won minus summed context win-probability
(volume-correct, additive — the `npg_minus_npxg` pattern for physical
duels).

## Usage

``` r
compute_duel_woe(events, duel_model, by_match = FALSE)
```

## Arguments

- events:

  Full per-league Opta events.

- duel_model:

  Fitted xDuel model.

- by_match:

  Logical. One row per player-(team-)match if TRUE.

## Value

data.table keyed by player (team, match) with `<prefix>_won/_exp/_woe`
for prefixes aerial, aerial_poss, takeon, tackle_poss, containment.
Per-90 normalisation is applied by the caller (which holds minutes).

## See also

Other xduel:
[`compute_all_duel_preps()`](https://peteowen1.github.io/panna/reference/compute_all_duel_preps.md),
[`fit_duel_model()`](https://peteowen1.github.io/panna/reference/fit_duel_model.md),
[`load_duel_model()`](https://peteowen1.github.io/panna/reference/load_duel_model.md),
[`predict_duel()`](https://peteowen1.github.io/panna/reference/predict_duel.md),
[`prepare_duels_from_events()`](https://peteowen1.github.io/panna/reference/prepare_duels_from_events.md),
[`save_duel_model()`](https://peteowen1.github.io/panna/reference/save_duel_model.md)
