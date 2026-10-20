# Abort unless a skill-SPM prior is tagged with the current sign convention

[`fit_career_rapm()`](https://peteowen1.github.io/panna/reference/fit_career_rapm.md)
negates `defense_spm` on the way in because it assumes positive = good
(Pete, 2026-09-04). A prior fitted under the old convention (negative =
good) is therefore inverted a second time, which pulls every good
defender toward a "bad defender" prior with no error. That happened:
`03_skill_spm_asof.rds` was fitted 2026-09-03, the day before the flip,
and every `career_panna_asof.parquet` snapshot since was shrunk toward
it (panna#257, found 2026-10-18; Spearman of its defence against the
live prior's was -0.81). The tag is the only reliable signal: a prior's
own values cannot say which way round they are.

## Usage

``` r
.assert_skill_spm_sign_convention(spm, source_desc)
```

## Arguments

- spm:

  One skill-SPM fit (an element of `03_skill_spm_asof.rds`, or
  `03_skill_spm.rds`).

- source_desc:

  Where it came from, for the error message.

## Value

Invisibly `TRUE` if tagged `CAREER_PANNA_SIGN_CONVENTION`.
