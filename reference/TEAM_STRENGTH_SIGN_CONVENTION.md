# Sign convention tag written to team_season_strength.parquet

Positive=good migration (docs/plans/SIGN-CONVENTION-POSITIVE-IS-GOOD.md
at the pannaverse root): `07c_team_season_strength.R` stamps this into a
`sign_convention` column so every consumer of `def_rating`
(`07_train_psr_model.R`, `build_epr_weekly.R`, `R/psv_opponent.R`) can
abort on an unmarked or mismatched file instead of silently reading an
inverted value. Value flipped to "defense_positive_good" in the SAME
commit as
[`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md)/[`extract_xrapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_xrapm_ratings.md)
negating `def_coefs` at extraction (2026-09-04) – any
`team_season_strength.parquet` on disk from before that commit is now
correctly rejected by every consumer until `07c` regenerates it.

## Usage

``` r
TEAM_STRENGTH_SIGN_CONVENTION
```

## Format

An object of class `character` of length 1.
