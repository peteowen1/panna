# Precompute the third-place slot allocation lookup (internal)

For every C(12, 8) = 495 combination of groups whose third-placed team
qualifies, finds an assignment of those 8 groups to the 8 designated R32
third-place slots that respects each slot's eligible-group list
(backtracking, most-constrained slot first). FIFA's regulations pin one
specific assignment per combination in an annex table; any
eligibility-respecting assignment has the same bracket geometry up to
which allowed third a winner draws, so this canonical solution is used
as a close approximation of the official table.

## Usage

``` r
build_third_allocation(slot_cands)
```

## Arguments

- slot_cands:

  List of integer vectors (group indices 1..12), the eligible groups per
  third slot, in R32 match order.

## Value

Integer matrix with 2^12 rows (indexed by bitmask of qualified
groups + 1) and one column per slot; valid rows hold the assigned group
index per slot, all other rows are NA.
