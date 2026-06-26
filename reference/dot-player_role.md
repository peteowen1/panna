# Player role for within-position normalization (broad GK/DEF/MID/FWD bucket)

Broad buckets align with career-panna (RAPM) as well as the finer
16-role (Spearman 0.613 vs 0.615) without needing a `position_side` the
PSR skills tables lack. PREFERS
[`classify_role()`](https://peteowen1.github.io/panna/reference/classify_role.md)
-\> broad when `position` + `position_side` are present (PSV
match-stats + the means artifact): it recognizes far more position
strings than the legacy `.simplify_position`, shrinking the "OTHER"
bucket (0.613 vs 0.595). Falls back to the modal `primary_position` (PSR
skills, also broad) / `pos_group`. Both branches emit the same
GK/DEF/MID/FWD labels, so artifact keys are consistent across paths.
Anything outside GK/DEF/MID/FWD -\> "OTHER".

## Usage

``` r
.player_role(dt)
```
