# Simulate Season

Runs Monte Carlo simulations of remaining fixtures to project final
league standings. Uses match-level win/draw/loss probabilities from the
prediction model.

## Usage

``` r
simulate_season(
  predictions,
  completed,
  n_sims = 10000L,
  points_win = 3L,
  points_draw = 1L,
  verbose = TRUE
)
```

## Arguments

- predictions:

  Data frame with columns: `home`, `away`, `prob_H`, `prob_D`, `prob_A`,
  and optionally `pred_home_goals`, `pred_away_goals`. Only unplayed
  fixtures should be included.

- completed:

  Data frame of completed matches with columns: `home`, `away`,
  `home_goals`, `away_goals`. These results are fixed across all
  simulations.

- n_sims:

  Integer. Number of Monte Carlo simulations. Default 10000.

- points_win:

  Integer. Points for a win. Default 3.

- points_draw:

  Integer. Points for a draw. Default 1.

- verbose:

  Logical. Print progress. Default TRUE.

## Value

A list with:

- `table`: Summary table with mean points, title/UCL/relegation
  probabilities

- `simulations`: Raw simulation results (n_sims x n_teams matrix of
  points)

- `positions`: Position frequency matrix (n_teams x n_teams)

## Examples

``` r
if (FALSE) { # \dontrun{
# Predictions for remaining fixtures
preds <- data.frame(
  home = c("Arsenal", "Liverpool"),
  away = c("Chelsea", "Man City"),
  prob_H = c(0.45, 0.35),
  prob_D = c(0.28, 0.30),
  prob_A = c(0.27, 0.35)
)
# Already-played matches
completed <- data.frame(
  home = "Arsenal", away = "Liverpool",
  home_goals = 2, away_goals = 1
)
result <- simulate_season(preds, completed, n_sims = 1000)
result$table
} # }
```
