# Every position grouping from one Opta position and side

The single mapping behind the `role_full`, `role16`, `role8` and `role4`
groupings. `role_full` keeps left and right (LCB, RCB, LCM, ...);
`role16` is
[`classify_role()`](https://peteowen1.github.io/panna/reference/classify_role.md)'s
16 roles; `role8` mirrors left and right into GK, CB, FB, DM, CM, AM, W,
ST (the default grouping); `role4` is GK, DEF, MID, FWD. A blank
position, "Substitute" or a missing side gives `NA` in every column.

## Usage

``` r
position_roles(position, position_side)
```

## Arguments

- position, position_side:

  Opta `position` and `position_side`.

## Value

A data.table with `role_full`, `role16`, `role8`, `role4`, one row per
input element.

## See also

Other positions:
[`position_asof()`](https://peteowen1.github.io/panna/reference/position_asof.md),
[`position_career()`](https://peteowen1.github.io/panna/reference/position_career.md),
[`position_match()`](https://peteowen1.github.io/panna/reference/position_match.md),
[`position_season()`](https://peteowen1.github.io/panna/reference/position_season.md),
[`position_slot_table()`](https://peteowen1.github.io/panna/reference/position_slot_table.md)
