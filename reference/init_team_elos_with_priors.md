# Build Initial-Elo Vector With Confederation Priors

Returns a named-vector starting Elo for each team. Teams whose
confederation is in `conf_priors` get the confederation's prior; teams
whose confederation is unknown (NA in lookup) get `initial_elo` (default
1500).

## Usage

``` r
init_team_elos_with_priors(
  teams,
  conf_lookup,
  conf_priors = ELO_CONFEDERATION_PRIORS,
  initial_elo = 1500
)
```

## Arguments

- teams:

  Character vector of team names.

- conf_lookup:

  Named character vector built by
  [`build_team_confederations()`](https://peteowen1.github.io/panna/reference/build_team_confederations.md).

- conf_priors:

  Named numeric vector (confederation -\> initial Elo). Default
  ELO_CONFEDERATION_PRIORS.

- initial_elo:

  Fallback for teams whose conf is unknown.

## Value

Named numeric vector – same length as `teams` (after dropping NA names).
