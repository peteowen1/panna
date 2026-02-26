# Adjust match stats for opponent quality, venue, and league level

Normalizes raw match-level stats to account for contextual factors
before they enter the skill estimation pipeline. Applying these
adjustments before decay-weighting produces cleaner skill estimates.

## Usage

``` r
adjust_match_stats_for_context(
  match_stats,
  elo_ratings = NULL,
  adjust_opponent = TRUE,
  adjust_venue = TRUE,
  adjust_league = TRUE,
  big5_leagues = c("EPL", "La_Liga", "Bundesliga", "Serie_A", "Ligue_1", "ENG", "ESP",
    "GER", "ITA", "FRA")
)
```

## Arguments

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md)
  with columns: match_id, player_id, team_name, opponent_team, is_home,
  competition, total_minutes, and stat columns.

- elo_ratings:

  Optional named numeric vector of team Elo ratings. If NULL, opponent
  adjustment is skipped.

- adjust_opponent:

  Logical, apply opponent strength adjustment (default TRUE).

- adjust_venue:

  Logical, apply home/away adjustment (default TRUE).

- adjust_league:

  Logical, apply cross-league adjustment (default TRUE).

- big5_leagues:

  Character vector of Big 5 league codes for reference level.

## Value

The input data.table with stat columns adjusted in place.

## Details

Three adjustments are applied multiplicatively:

1.  **Opponent strength**: Offensive stats scaled by
    `league_avg_elo / opponent_elo`. Harder opponents inflate your
    stats; easier opponents deflate them.

2.  **Home/away**: Per-stat home/away differential computed from the
    full dataset. Away stats multiplied up to home-equivalent.

3.  **League quality**: Cross-league normalization using Big 5 average
    as reference level.
