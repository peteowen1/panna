# Build final match results (goal counts) from raw Opta events

Derives `home_goals`/`away_goals` per match by counting type-16 (Goal)
events, for use as WP model training labels.

## Usage

``` r
.build_match_results_from_events(events, lineups)
```

## Arguments

- events:

  Raw Opta match events with `match_id`, `type_id`, `team_id`, and (for
  own-goal detection) `qualifier_json` or `type_name`.

- lineups:

  Opta lineups with `team_position` or `is_home`, to determine the
  home/away team per match.

## Value

A data.frame with `match_id`, `home_team_id`, `away_team_id`,
`home_goals`, `away_goals`.

## Details

Opta logs an own goal as a type-16 event attributed to the OWN-SCORER's
team (qualifier 28 – see `OPTA_REFERENCE.md` and the `is_own_goal`
convention in
[`parse_opta_qualifiers`](https://peteowen1.github.io/panna/reference/parse_opta_qualifiers.md)/`convert_opta_to_spadl`).
The scoreboard credit belongs to the OPPOSING team, so own-goal events
are flipped to the other team in the match before tallying – otherwise
every own-goal match produces an inverted scoreline and win/draw/loss
label (H2-OG-WP). This was previously duplicated (and buggy) in
`data-raw/epv/05_train_wp_model.R` and
`data-raw/epv/06_calculate_wpa.R`; both now call this shared helper.
