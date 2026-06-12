# Prepare Opta Lineups for Splint Creation

Converts Opta lineup data to the format expected by splint creation
functions. Opta lineups have: is_starter, minutes_played, sub_on_minute,
sub_off_minute.

## Usage

``` r
prepare_opta_lineups_for_splints(opta_lineups, player_timing = NULL)
```

## Arguments

- opta_lineups:

  Data frame from load_opta_lineups()

- player_timing:

  Optional data frame from
  [`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md)
  with columns `match_id`, `player_id`, `on_minute`, `off_minute`,
  `is_starter`. When supplied, chain-derived on/off times override the
  lineup-derived ones.

## Value

Data frame with columns: match_id, player_id, player_name, team,
is_home, is_starter, minutes, on_minute, off_minute, on_off_source. The
`on_off_source` column is "chain" when chain-derived times were used,
"lineup" otherwise.

## Details

When `player_timing` is supplied (chain-derived from
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md)),
it OVERRIDES the lineup-derived on/off times for any (match_id,
player_id) pair present in the timing table. Chain timing is preferred
because Opta lineups record `minutes_played = 90` for unsubstituted
finishers regardless of stoppage time, and round sub minutes to integers
– chains carry second-level precision and the real final-whistle time.
