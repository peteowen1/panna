# Build league fixed-effect dummy columns for SPM

SPM maps per-90 box-score rates onto RAPM. RAPM is already
opponent-adjusted at player level (the design matrix carries
`_off`/`_def` columns for both teams) and league-season centred, but the
box-score rates it is regressed on are neither. What survives is
residual stat inflation: the same per-90 line means less in a weaker
league. Measured 2026-09-02 on 22,755 players, the fitted league effect
spans **0.95 sd of RAPM** end to end (EPL to CAF_CL), worth ~3.3\\ as
many as the Bundesliga (9 EPL / 2 Saudi once the term is added).

## Usage

``` r
.spm_league_shares(stats, min_n = 50, prefix = "lgshare_")
```

## Arguments

- min_n:

  Minimum rows for a league to get its own dummy (default 50). Thinner
  leagues fold into the reference level rather than fitting a
  coefficient on a handful of players.

- data:

  Data frame carrying a `competition` or `league` column.

- levels:

  Character vector of league levels from the fitted model. When `NULL`
  they are derived from `data` (levels with at least `min_n` rows),
  which is the fit-time path.

## Value

A list with `data` (input plus dummy columns), `levels` (the
non-reference levels, for storing in model metadata) and `cols` (the
dummy column names).

## Details

Dummies are 0/1 with one level held out as the reference, so an **unseen
league at predict time gets all-zero dummies and falls back to that
reference** rather than erroring. That is the deliberate behaviour: a
new competition should be priced as the reference league, not dropped.
