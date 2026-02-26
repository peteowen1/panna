# Create splint boundaries

Defines start and end times for each splint based on events and
optionally shots. Splints are created at:

- Start of match (minute 0)

- Each goal

- Each substitution

- Each red card

- Half time (minute 45)

- End of match (dynamically calculated from stoppage time in
  events/shots)

## Usage

``` r
create_splint_boundaries(
  events,
  shots = NULL,
  include_goals = TRUE,
  include_halftime = TRUE
)
```

## Arguments

- events:

  Data frame of match events (with minute and optionally added_time
  columns)

- shots:

  Data frame of shots (optional, with minute column for stoppage time)

- include_goals:

  Logical, whether to create new splints at goals (default TRUE)

- include_halftime:

  Logical, whether to create splint at halftime (default TRUE)

## Value

Data frame of splint boundaries with game state

## Details

Uses continuous time where second-half events are offset by first-half
stoppage. Example with 3 mins first-half stoppage and 11 mins
second-half stoppage:

- First half: 0 to 48 (45 + 3)

- Second half: 48 to 104 (offset by 3, plus 90+11)

Also tracks game state (cumulative goals, red cards, player counts) at
each splint boundary.
