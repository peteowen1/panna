# Summarize Match-Level Possession Chains

Aggregates possession chain data at the match-team level, providing
chain counts, success rates, territory metrics, and duration stats.

## Usage

``` r
summarize_match_chains(spadl_with_chains)
```

## Arguments

- spadl_with_chains:

  SPADL actions with chain_id and chain outcome columns (from
  [`create_possession_chains()`](https://peteowen1.github.io/panna/reference/create_possession_chains.md) +
  [`label_actions_with_outcomes()`](https://peteowen1.github.io/panna/reference/label_actions_with_outcomes.md)).

## Value

Data frame with one row per team per match containing:

- `match_id`, `team_id`: Identifiers

- `total_chains`: Number of possession chains

- `chains_with_shot`, `chains_with_goal`: Chain outcomes

- `avg_chain_length`: Mean actions per chain

- `avg_chain_duration`: Mean chain duration (seconds)

- `territory_pct`: Percentage of chains reaching final third (x \> 66)

- `chain_xg`: Sum of xG across all chains (if available)

- `possession_pct`: Team's share of total chains in the match

## Examples

``` r
if (FALSE) { # \dontrun{
spadl <- convert_opta_to_spadl(events)
spadl <- create_possession_chains(spadl)
outcomes <- classify_chain_outcomes(spadl)
spadl <- label_actions_with_outcomes(spadl, outcomes)
match_chains <- summarize_match_chains(spadl)
} # }
```
