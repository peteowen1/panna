# Actions treated as stopping a shot

SPADL has no block type: 2,802 of 5,124 `keeper_save` rows (54.7%) are
outfield players, which are blocks. So one action type covers both,
which is why extending the rule from keepers to all shot-stoppers costs
nothing. `keeper_claim`, `keeper_punch` and `keeper_pick_up` are
deliberately absent – all three already read positive, and changing
something already correct is a correctness note, not a fix.

## Usage

``` r
NG_STOP_ACTIONS
```

## Format

An object of class `character` of length 1.
