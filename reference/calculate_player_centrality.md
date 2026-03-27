# Calculate Player Centrality

Builds a player interaction network from match data and computes
centrality centrality scores. Players who face diverse, high-quality
opponents get higher centrality. Players isolated in weak leagues get
lower centrality.

## Usage

``` r
calculate_player_centrality(
  player_matches,
  min_matches = 5L,
  damping = 0.85,
  max_iter = 100L,
  tol = 1e-06
)
```

## Arguments

- player_matches:

  Data frame with columns:

  - `player_id`: Player identifier

  - `team`: Player's team

  - `opponent`: Opposing team

  - `match_id`: Match identifier

  - `minutes` (optional): Minutes played (used as weight)

- min_matches:

  Integer. Minimum matches for inclusion. Default 5.

- damping:

  Numeric. centrality damping factor (0-1). Default 0.85.

- max_iter:

  Integer. Maximum centrality iterations. Default 100.

- tol:

  Numeric. Convergence tolerance. Default 1e-6.

## Value

Data frame with player_id, centrality (0-1), unique_opponents,
matches_played, component_id, component_size

## Examples

``` r
if (FALSE) { # \dontrun{
# Build from splint/lineup data
player_matches <- data.frame(
  player_id = c("p1", "p1", "p2", "p2"),
  team = c("Arsenal", "Arsenal", "Chelsea", "Chelsea"),
  opponent = c("Chelsea", "Liverpool", "Arsenal", "Man City"),
  match_id = c("m1", "m2", "m1", "m3"),
  minutes = c(90, 75, 90, 80)
)
centrality <- calculate_player_centrality(player_matches)
} # }
```
