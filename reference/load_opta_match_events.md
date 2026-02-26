# Load Opta All Match Events (All Events with X/Y Coordinates)

Loads ALL match events with x/y coordinates from Opta/TheAnalyst data.
Each match typically has ~2000 events including passes, tackles,
aerials, dribbles, shots, and more. This is the most comprehensive event
data available.

## Usage

``` r
load_opta_match_events(
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

Data frame of all match events with columns:

- match_id: Match identifier

- event_id: Unique event identifier

- type_id: Opta event type (1=pass, 3=dribble, 7=tackle, 13-16=shots,
  44=aerial, etc.)

- player_id, player_name: Player involved

- team_id: Team that performed the action

- minute, second: Time of event

- x, y: Start coordinates (0-100 scale)

- end_x, end_y: End coordinates for passes/carries (0-100 scale)

- outcome: 1=successful, 0=unsuccessful

- period_id: 1=first half, 2=second half

- qualifier_json: Full qualifiers as JSON string for advanced analysis

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all EPL match events
epl_events <- load_opta_match_events("ENG", season = "2024-2025")

# Filter to just passes (type_id = 1)
passes <- epl_events |>
  dplyr::filter(type_id == 1)

# Build passing networks
pass_counts <- passes |>
  dplyr::filter(outcome == 1) |>
  dplyr::count(match_id, player_id)

# Filter to tackles (type_id = 7)
tackles <- epl_events |>
  dplyr::filter(type_id == 7)
} # }
```
