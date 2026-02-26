# Build sparse player matrix from triplets

Constructs the sparse matrix encoding which players are on
offense/defense in each row, including replacement-level columns.

## Usage

``` r
.build_rapm_sparse_matrix(
  players,
  valid_splints,
  player_ids,
  replacement_player_ids,
  n_rows
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

## Value

List with X_players (sparse matrix), col_names, n_player_cols,
replacement_off_appearances, replacement_def_appearances
