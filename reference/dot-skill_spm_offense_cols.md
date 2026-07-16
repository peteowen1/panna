# Canonical offense predictor columns for the SKILL-based SPM

Skill-SPM (`estimated-skills/03_skill_spm.R`, and the expanding-window
as-of variant in `R/spm_asof.R`) trains on decay-weighted skill features
rather than raw box-score aggregates, so its offense/defense
hand-curated column lists are a DIFFERENT (smaller) set than the
box-score SPM's (`05_spm.R` /
[`.spm_opta_predictor_cols()`](https://peteowen1.github.io/panna/reference/dot-spm_opta_predictor_cols.md))
– some raw box columns (e.g. `hit_woodwork_p90`, `att_pen_goal_p90`)
aren't carried as skill features. Extracted to ONE place so the
all-history fit (`03_skill_spm.R` section 10) and the expanding-window
per-year fits
([`fit_expanding_skill_spm()`](https://peteowen1.github.io/panna/reference/fit_expanding_skill_spm.md))
can never drift apart – hand-copied O/D feature lists are a recurring
drift bug in this repo (see
[`.spm_opta_predictor_cols()`](https://peteowen1.github.io/panna/reference/dot-spm_opta_predictor_cols.md)'s
own history).

## Usage

``` r
.skill_spm_offense_cols(data)
```

## Arguments

- data:

  Data frame of candidate features (e.g. `spm_train_data`)

## Value

Character vector of offense predictor columns present in `data`
