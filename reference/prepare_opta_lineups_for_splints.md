# Prepare Opta Lineups for Splint Creation

Converts Opta lineup data to the format expected by splint creation
functions. Opta lineups have: is_starter, minutes_played, sub_on_minute,
sub_off_minute.

## Usage

``` r
prepare_opta_lineups_for_splints(opta_lineups)
```

## Arguments

- opta_lineups:

  Data frame from load_opta_lineups()

## Value

Data frame with columns: match_id, player_id, player_name, team,
is_home, is_starter, minutes, on_minute, off_minute
