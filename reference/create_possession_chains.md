# Create Possession Chains from SPADL Actions

Groups consecutive actions by the same team into possession chains.
Optimized with data.table for fast processing of large datasets.

## Usage

``` r
create_possession_chains(spadl_actions)
```

## Arguments

- spadl_actions:

  Data frame in SPADL format from convert_opta_to_spadl()

## Value

Data frame with chain assignments added

## Details

A chain ends when:

- Opponent gains possession (successful tackle/interception)

- Ball goes out of play (foul, throw-in situation)

- Goal is scored

- Period ends

- Large time gap (\>30 seconds)

## Examples

``` r
if (FALSE) { # \dontrun{
spadl <- convert_opta_to_spadl(opta_events)
chains <- create_possession_chains(spadl)
head(chains[, c("match_id", "chain_id", "action_type", "team_id")])
} # }
```
