# The shot aftermath line used when a build has too few shots to fit its own

`A = intercept + slope * xG`, fitted on ENG 2024-2025 (8,709 non-goal
shots, 2026-09-23). A league-season with fewer than 50 non-goal shots (a
small tournament) uses this line rather than dropping back to plain xG,
so every league in one published file prices shots by the same rule.

## Usage

``` r
NG_SHOT_AFTERMATH_LINE
```

## Format

An object of class `list` of length 4.
