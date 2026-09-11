# Abort if a team_season_strength.parquet table isn't sign-tagged as expected

Every reader of `def_rating`/`opp_def_rating` (`07_train_psr_model.R`,
`build_epr_weekly.R`, and any future `psv_opponent.R` caller) must call
this right after loading the parquet, so a file built before the
`sign_convention` column existed – or built under the OTHER convention,
post-migration – aborts loudly instead of silently training on an
inverted `def_rating`. See
`docs/plans/SIGN-CONVENTION-POSITIVE-IS-GOOD.md`.

## Usage

``` r
.assert_team_strength_sign_convention(ts, source_desc)
```

## Arguments

- ts:

  The team_season_strength data.frame/data.table, already loaded.

- source_desc:

  Short string naming the caller, for the error message.
