# Summarize Player-Level Possession Chains

Aggregates how each player contributes to possession chains within a
match. Tracks chain involvement, starts, finishes, and progressive
contributions.

## Usage

``` r
summarize_player_chains(spadl_with_chains)
```

## Arguments

- spadl_with_chains:

  SPADL actions with chain_id and chain outcome columns (from
  [`create_possession_chains()`](https://peteowen1.github.io/panna/reference/create_possession_chains.md) +
  [`label_actions_with_outcomes()`](https://peteowen1.github.io/panna/reference/label_actions_with_outcomes.md)).

## Value

Data frame with one row per player per match containing:

- `match_id`, `player_id`, `player_name`, `team_id`

- `chains_involved`: Unique chains the player participated in

- `chain_starts`: Chains where player had the first action

- `chain_finishes`: Chains where player had the last action before
  outcome

- `progressive_chains`: Chains where player advanced ball \>25 units
  forward

- `key_chain_actions`: Actions in chains ending in shot/goal

## Examples

``` r
if (FALSE) { # \dontrun{
spadl <- convert_opta_to_spadl(events)
spadl <- create_possession_chains(spadl)
outcomes <- classify_chain_outcomes(spadl)
spadl <- label_actions_with_outcomes(spadl, outcomes)
player_chains <- summarize_player_chains(spadl)
} # }
```
