# Reliability-shrunk PSV display scale ("expected GD contribution per 90")

Multiplies the lambda-priced display PSV
(`calculate_psv(reliability = )`) so its units become "expected
goal-difference contribution per 90". Re-derived 2026-08-14 (was 5.134)
by regressing match goal difference on minutes-weighted team sums of
lambda-priced player PSV: 13,548 matches, R-squared = 0.156,
t(c_outfield) = 40.2 – the slope makes summed player PSVs predict match
GD with slope 1.

## Usage

``` r
PSV_RELIABILITY_GD_SCALE
```

## Format

Numeric value: 2.717

## Details

The 0.31 quoted in 07d's header is a hardcoded reading from the
2026-07-20 run, and the coefficients were retrained THREE times after it
while nothing re-derived this constant. Measured by re-running 07d
against each vintage with everything else held identical (same matches,
same `psv_match_reliability.csv` – unchanged since ffa549f – same
SPLIT_DATE, same n = 13,548):

      coefficients                                c_outfield   R^2     t
      2026-07-20 (pre f9c7e31/bd34465)                 5.134   0.31   ~59
      bd34465 (last pre-join-fix retrain)              4.888   0.142  37.2
      7b34f51 (post join fix)                          2.717   0.156  40.2

(The middle row was measured from 86d3e9e's tree, which is not itself a
retrain – it carries bd34465's coefficients unchanged, since nothing
between them touched inst/extdata. The vintage is bd34465's.)

So the join fix moved fit quality UP (0.142 -\> 0.156, t 37.2 -\> 40.2),
which is the expected direction for a data-bug fix; it cut the SLOPE by
~44% (4.888 -\> 2.717) because the corrected PSR effects are ~3x larger,
so summed PSVs need a smaller multiplier to reach the same GD. The 54%
relative R-squared drop happened in the 2026-07-21 retrains – most
plausibly f9c7e31, which removed the zonal finishing features that had
been supplying large and partly spurious variance – and went unnoticed
for 3.5 weeks. That also means 5.134 was stale from 2026-07-21 (it
should have been ~4.888) independently of the join bug: BOTH July
retrains changed the coefficients this constant is fit against, and
neither re-ran 07d.

ONE global constant for BOTH the outfield and GK populations: the
GK-specific GD coefficient (c = 0.305 in this fit) is again REJECTED as
pricing the \#159 team-context leak in the GK reliability artifact, not
genuine keeper skill – GKs use this same constant until \#159 retrains.

Re-derive via `data-raw/estimated-skills/07d_derive_psv_gd_scale.R`
after any retrain of `psv_match_reliability.csv` (07b) or the PSR/PSV
coefficients (07_train_psr_model.R), then update this value by hand –
the script writes no file, it only prints the number. Note 07b reads
neither the coefficients nor this constant, so a coefficient-only
retrain does not require re-running it.

## See also

Other psr:
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
