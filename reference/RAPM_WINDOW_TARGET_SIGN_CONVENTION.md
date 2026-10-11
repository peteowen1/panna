# Sign convention stamped into `rapm_window_targets.rds` (the SPM panel target)

Third member of the same family, and the one that actually bit
(panna#256). `04b_rapm_window_targets.R` builds windowed prior-free RAPM
targets; those feed `04c_build_spm_panel.R` -\> `spm_panel.rds` -\> the
S6 SPM override -\> the **xRAPM defensive prior**. The cached target was
written 2026-07-21 and the positive=good flip landed 2026-09-04, so
every vintage in it is still negative=good while `05_spm.R` and
`07_seasonal_ratings.R` both assert it is positive=good. Measured
consequence: the defensive prior reached
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
inverted for **78.7%** of 2026 player-seasons, and xRAPM-vs-RAPM
defensive agreement fell to 0.622 at under 450 minutes against a control
that held flat at 0.954.

## Usage

``` r
RAPM_WINDOW_TARGET_SIGN_CONVENTION
```

## Format

An object of class `character` of length 1.

## Details

Nothing caught it because, exactly as with `career_rapm.parquet`, there
was no tag to check. Stamped at write time by 04b;
`.assert_window_target_sign_convention()` (`R/spm_panel.R`) aborts on a
wrongly-tagged file and migrates an untagged (pre-flip) one loudly
rather than silently trusting it.
