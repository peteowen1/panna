# Columns of 01_match_stats that predictions steps 02, 02b and 10b's resolvers read

The union of what step 02 and step 02b keep after
[`.narrow_match_stats_for_skills()`](https://peteowen1.github.io/panna/reference/dot-narrow_match_stats_for_skills.md),
the columns step 02 reads before it narrows (`position_side` for role8)
and the columns 10b's position resolvers read. Narrowing this table
again gives the same columns as narrowing the full one, because
skill-column detection is by name.

## Usage

``` r
.predictions_match_stats_cols(match_stats)
```
