# Calculate splint-level xG differential

Calculates the non-penalty xG differential for each splint.

## Usage

``` r
calculate_splint_npxgd(
  boundaries,
  shooting,
  match_id,
  home_team = NULL,
  away_team = NULL
)
```

## Arguments

- boundaries:

  Data frame of splint boundaries

- shooting:

  Processed shooting data

- match_id:

  Match identifier

- home_team:

  Name of the home team

- away_team:

  Name of the away team

## Value

Data frame with npxGD for each splint
