# Build a full-stream adjacency table

Answers "what happened next" from the **complete** Opta event stream,
before any SPADL filtering, and returns it keyed on `event_id` so the
ledger can join it to SPADL actions via `original_event_id`.

## Usage

``` r
ng_build_adjacency(events, verbose = TRUE)
```

## Arguments

- events:

  Raw Opta events, as returned by
  [`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md).
  Needs `match_id`, `event_id`, `type_id`, `team_id`, `player_id`,
  `period_id`, `minute`, `second`.

- verbose:

  Print a summary of what the table found. Default `TRUE`.

## Value

A data.table keyed on `match_id` + `event_id`, one row per event, with:

- next_team_id:

  team on the next *visible* event (attribution types and real actions;
  markers are skipped)

- next_player_id:

  player on that event

- next_type_id:

  its Opta type id

- gap_type_id:

  if the next visible event is one of `NG_ATTRIBUTION_TYPES`, its id –
  otherwise `NA`. This is the row SPADL drops and the ledger would
  otherwise step over.

- gap_player_id:

  the player named on that dropped row

- true_possession_change:

  next visible event belongs to the other team

## Details

Nothing about the EPV model changes:
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md),
the chains, the features and
[`calculate_action_epv()`](https://peteowen1.github.io/panna/reference/calculate_action_epv.md)
all keep seeing exactly what they see today. Only the ledger's view of
"who was next" is corrected. This is deliberate – moving the filter
itself would shift the model's inputs and every measurement taken
against them.

## See also

Other net_goals:
[`ng_adjust_for_rating()`](https://peteowen1.github.io/panna/reference/ng_adjust_for_rating.md),
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md),
[`ng_check_team_totals()`](https://peteowen1.github.io/panna/reference/ng_check_team_totals.md),
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md),
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
