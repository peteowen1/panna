# Sign convention tag written to career_panna.parquet / career_panna_asof.parquet

Same idiom as `TEAM_STRENGTH_SIGN_CONVENTION`, added after panna#F1
(2026-09-07, fixed 2026-09-11): commit `795feeb1` removed the
defense-sign negation at every export site that reads
`career_panna.parquet`, on the assumption the file already stored
`panna_defense` as positive=good. It didn't – the release asset was 7
weeks stale – so every consumer that stopped flipping inherited the OLD
(negative=good) convention and published elite defenders (Rodri, Saliba,
Gabriel Magalhães) as the worst in the game. `09_career_panna.R` /
`09b_career_panna_asof.R` stamp this into a `sign_convention` column at
write time; every consumer must call
[`.assert_career_panna_sign_convention()`](https://peteowen1.github.io/panna/reference/dot-assert_career_panna_sign_convention.md)
(`R/career_rapm.R`) right after loading either file, so a stale-vintage
file aborts loudly instead of silently shipping inverted again.

## Usage

``` r
CAREER_PANNA_SIGN_CONVENTION
```

## Format

An object of class `character` of length 1.
