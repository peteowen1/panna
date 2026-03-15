# Plot All Chains for a Match

Draws all possession chains for a match (or one team) as
semi-transparent paths on a pitch. Chains ending in goals are
highlighted.

## Usage

``` r
plot_match_chains(
  spadl_with_chains,
  target_match_id,
  target_team_id = NULL,
  highlight_goals = TRUE,
  background = c("white", "green")
)
```

## Arguments

- spadl_with_chains:

  SPADL actions with chain_id and optionally chain_outcome columns.

- target_match_id:

  Character. Match ID to filter.

- target_team_id:

  Character. Optional team ID to show only one team's chains.

- highlight_goals:

  Logical. Highlight chains ending in goals (default TRUE).

- background:

  Character. Pitch background: "white" (default) or "green".

## Value

A ggplot2 object showing possession chains on a pitch.

## Examples

``` r
if (FALSE) { # \dontrun{
spadl <- create_possession_chains(convert_opta_to_spadl(events))
outcomes <- classify_chain_outcomes(spadl)
spadl <- label_actions_with_outcomes(spadl, outcomes)
plot_match_chains(spadl, match_id = "abc123")
plot_match_chains(spadl, match_id = "abc123", team_id = "team1")
} # }
```
