# Attach a per-action red_card flag to possession chains (#93)

SPADL carries only on-ball gameplay actions, so card events (Opta
`type_id == 17`) are filtered out in
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
and never reach the possession chains. As a result the red-card block in
[`create_wp_features`](https://peteowen1.github.io/panna/reference/create_wp_features.md)
(`if ("red_card" %in% names(dt))`) always took its else branch and
`red_card_diff` was a constant 0 – a dead feature. This helper
re-derives red cards from the RAW events and joins a 0/1 `red_card`
column onto the chains so that block activates.

## Usage

``` r
add_red_card_to_chains(chains, events)
```

## Arguments

- chains:

  Possession chains (output of
  [`create_possession_chains()`](https://peteowen1.github.io/panna/reference/create_possession_chains.md)).
  Must contain `match_id`, `team_id`, `time_seconds`, `period_id`.

- events:

  Raw Opta match events with `match_id`, `type_id`, `team_id`, `minute`,
  `qualifier_json` (and optionally `second`, `period_id`).

## Value

`chains` with an integer `red_card` column (1 on the action nearest each
carded team's red-card time, else 0). If no reds are detected (or
required columns are missing) every row gets `red_card = 0`, which
reproduces the previous constant-0 behaviour for that match.

## Details

Detection mirrors
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md)
in `splint_creation.R`: `type_id == 17` (Card) carrying qualifier 33
(straight red) or 14 (second yellow). The earliest such card per (match,
team) is taken, and the flag is set on the single chain action of the
carded team nearest that card's time (matching the SPADL clock,
`time_seconds = minute*60 + second`). One flagged action per red card is
exactly what `create_wp_features`'
[`cumsum()`](https://rdrr.io/r/base/cumsum.html) logic expects.
