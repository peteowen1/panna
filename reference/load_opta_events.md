# Load Opta Match Events (Goals, Cards, Substitutions)

Loads match events including goals, cards, and substitutions with
timing. Useful for creating splint boundaries in RAPM calculations.

## Usage

``` r
load_opta_events(
  league,
  season = NULL,
  columns = NULL,
  source = c("remote", "local")
)
```

## Arguments

- league:

  League code. Accepts panna format (ENG, ESP, GER, ITA, FRA) or Opta
  format (EPL, La_Liga, Bundesliga, Serie_A, Ligue_1).

- season:

  Optional season filter (e.g., "2021-2022"). If NULL, loads all
  seasons.

- columns:

  Optional character vector of columns to select. If NULL, selects all.

- source:

  Data source: "remote" (default) downloads from GitHub releases,
  "local" loads from local files (requires prior
  [`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)).

## Value

Data frame of match events with columns:

- match_id: Match identifier

- event_type: goal, yellow_card, red_card, second_yellow, substitution

- minute, second: Time of event

- team_id: Team involved

- player_id, player_name: Player involved

- player_on_id, player_on_name: Substitute coming on (for substitutions)

- player_off_id, player_off_name: Player leaving (for substitutions)

- assist_player_id, assist_player_name: Assister (for goals)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load EPL match events
epl_events <- load_opta_events("ENG", season = "2024-2025")

# Filter to just red cards
red_cards <- epl_events |>
  dplyr::filter(event_type == "red_card")
} # }
```
