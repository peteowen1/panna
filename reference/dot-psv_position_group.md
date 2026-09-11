# Bucket a position label into the calibration's position groups

Accepts three input vocabularies, tried in order: an already-broad label
(`GK`/`DEF`/`MID`/`FWD`, passed through); a raw Opta match-role string
(`"Goalkeeper"`, `"Defender"`, ...), resolved via the canonical
[`.simplify_position`](https://peteowen1.github.io/panna/reference/dot-simplify_position.md)
rather than a second hand-rolled mapping (review finding, panna#211: an
earlier version of this function regex-matched the same vocabulary
independently, which is exactly the kind of duplicated classifier this
codebase's own gotchas warn drifts silently); or a 16-role
[`classify_role()`](https://peteowen1.github.io/panna/reference/classify_role.md)
code (`"CB"`, `"DM"`, ...), resolved via `.role16_to_broad` – needed
because
[`apply_psv_calibration()`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md)'s
own fallback chain can hand this function a fine-grained
`primary_position` (see
[`.player_role`](https://peteowen1.github.io/panna/reference/dot-player_role.md)'s
comment: that column is "usually already broad... but sometimes a
fine-grained label"). Anything none of the three recognize returns NA,
which
[`apply_psv_calibration()`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md)
treats as factor 1 (uncalibrated passthrough).

## Usage

``` r
.psv_position_group(p)
```
