# Assert the two entries for an action cancel

Under the team convention every action is booked twice – once to the
side that acted, once negated to the side that conceded – so its
payments sum to zero. That is what makes a match sum to zero and each
team to its own goal difference, with no reconciliation anywhere.

## Usage

``` r
.ng_assert_double_entry(dt, pay, tol = 1e-09)
```

## Arguments

- dt:

  Prepared actions with `epv_delta`

- pay:

  Payments from
  [`.ng_credit_terms_team()`](https://peteowen1.github.io/panna/reference/dot-ng_credit_terms_team.md)

- tol:

  Absolute tolerance per action and per side. Default 1e-9.
