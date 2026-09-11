# Abort if a career_panna(\_asof).parquet table isn't sign-tagged as expected

Every reader of `panna_defense` (`02_player_ratings_to_team.R`'s
career-Panna override, `09_export_ratings.R`,
`12d_export_domestic_team_strength.R`, `12_export_wc2026_blog.R`, and
any future caller) must call this right after loading either file, so a
file built before the `sign_convention` column existed – or a stale
release asset predating the 2026-09-04 sign-convention flip – aborts
loudly instead of silently reading an inverted `panna_defense` (see
panna#F1, 2026-09-07/11).

## Usage

``` r
.assert_career_panna_sign_convention(cp, source_desc)
```

## Arguments

- cp:

  The career_panna(\_asof) data.frame/data.table, already loaded.

- source_desc:

  Short string naming the caller, for the error message.
