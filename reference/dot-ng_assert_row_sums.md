# Assert that each action's payments sum to that action's value

The one property that makes the rules safe to change: a rule may move
value between recipients but can never create or destroy it, so a match
total is untouchable by definition. Checked against the action table,
upstream of any aggregation or reconciliation – torp's suite passed 48
assertions against a deliberately broken ledger because every one of
them ran downstream of a reconciler that forced the total.

## Usage

``` r
.ng_assert_row_sums(dt, pay, tol = 1e-09)
```

## Arguments

- dt:

  Prepared actions with `value_home`

- pay:

  Payments from
  [`.ng_credit_terms()`](https://peteowen1.github.io/panna/reference/dot-ng_credit_terms.md)

- tol:

  Absolute tolerance per action. Default 1e-9.
