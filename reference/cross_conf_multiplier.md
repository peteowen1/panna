# Compute Cross-Confederation Multiplier for a Match

Compute Cross-Confederation Multiplier for a Match

## Usage

``` r
cross_conf_multiplier(
  home_team,
  away_team,
  conf_lookup,
  mult = ELO_CROSS_CONF_MULT
)
```

## Arguments

- home_team, away_team:

  Team names.

- conf_lookup:

  Named character vector built by
  [`build_team_confederations()`](https://peteowen1.github.io/panna/reference/build_team_confederations.md).

- mult:

  Multiplier when the two teams are from different confederations.
  Default ELO_CROSS_CONF_MULT (1.5).

## Value

1.0 if same-conf or either team's conf is unknown, else `mult`.
