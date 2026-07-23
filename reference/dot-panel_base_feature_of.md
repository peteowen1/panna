# Recover the base feature name from a `dev__<role_group>__<feature>` design-matrix column name (role_group codes are alphanumeric-only, see `classify_role_group()`); non-deviation names pass through unchanged.

Recover the base feature name from a `dev__<role_group>__<feature>`
design-matrix column name (role_group codes are alphanumeric-only, see
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md));
non-deviation names pass through unchanged.

## Usage

``` r
.panel_base_feature_of(col_names)
```
