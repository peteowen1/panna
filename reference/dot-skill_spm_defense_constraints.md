# Sign-constraint feature lists for the SKILL-based SPM defense model

In the negative-is-good defense convention, "good defense" features must
get a non-positive SPM coefficient (more = better defender) and "bad
defense" features a non-negative one. Mirrors `05_spm.R`'s
`defense_good_features`/`defense_bad_features` for the box-score SPM,
restricted to the skill-SPM's smaller feature set.

## Usage

``` r
.skill_spm_defense_constraints()
```

## Value

List with `good` and `bad` character vectors of feature names.
