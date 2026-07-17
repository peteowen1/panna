# Build sparse player matrix from triplets

Constructs the sparse matrix encoding which players are on
offense/defense in each row (`"od"` mode, default), or a single signed
home/away column per player (`"net"` mode) – see `mode`. Includes
replacement-level columns.

## Usage

``` r
.build_rapm_sparse_matrix(
  players,
  valid_splints,
  player_ids,
  replacement_player_ids,
  n_rows,
  mode = c("od", "net")
)
```

## Arguments

- players:

  Data frame of player appearances

- valid_splints:

  Data frame of valid splints

- player_ids:

  Character vector of regular player IDs

- replacement_player_ids:

  Character vector of replacement player IDs

- n_rows:

  Total rows in design matrix

- mode:

  Design matrix mode. `"od"` (default) builds 2 columns per player
  (`_off`/`_def`) against 2 rows per splint – the production
  xg/goals/epv layout, byte-identical to before this parameter existed.
  `"net"` builds 1 column per player (`_net`) against 1 row per splint,
  valued `+share` when the player's team is home and `-share` when away
  (FABLE-PRIOR-FIX-PLAN.md D2).

## Value

List with X_players (sparse matrix), col_names, n_player_cols,
replacement_off_appearances, replacement_def_appearances
