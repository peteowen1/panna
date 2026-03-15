# Plot a Single Possession Chain

Draws a single chain's actions as connected arrows on a pitch,
color-coded by action type.

## Usage

``` r
plot_chain(
  spadl_with_chains,
  target_match_id,
  target_chain_id,
  background = c("white", "green")
)
```

## Arguments

- spadl_with_chains:

  SPADL actions with chain_id column.

- target_match_id:

  Character. Match ID to filter.

- target_chain_id:

  Integer. Chain ID within the match.

- background:

  Character. Pitch background: "white" (default) or "green".

## Value

A ggplot2 object showing the chain on a pitch.

## Examples

``` r
if (FALSE) { # \dontrun{
spadl <- create_possession_chains(convert_opta_to_spadl(events))
plot_chain(spadl, match_id = "abc123", chain_id = 5)
} # }
```
