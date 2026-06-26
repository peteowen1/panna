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
