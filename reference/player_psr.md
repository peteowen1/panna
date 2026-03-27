# Get Player Skill Ratings

Returns a ranked PSR leaderboard using pre-computed weekly snapshots.
Snaps to the nearest weekly date at or before `date`.

## Usage

``` r
player_psr(
  date = NULL,
  player = NULL,
  n = 50,
  position = NULL,
  target = c("xg", "goals"),
  source = c("remote", "local")
)
```

## Arguments

- date:

  Date to query as a `Date` or `"YYYY-MM-DD"` string. Defaults to the
  latest available snapshot.

- player:

  Optional player name filter (partial match, case-insensitive). E.g.,
  `"Salah"` matches "Mohamed Salah".

- n:

  Number of top players to show (default 50, NULL for all).

- position:

  Filter by position group: `"GK"`, `"DEF"`, `"MID"`, `"FWD"`, or NULL
  for all.

- target:

  One of `"xg"` (default, xG differential) or `"goals"` (goal
  differential). Note: weekly snapshots are xG-based; `"goals"`
  recomputes from skills on-demand (slower).

- source:

  Data source: `"remote"` (default, GitHub Releases) or `"local"`.

## Value

A data.table with columns: `snapshot_date`, `player_name`,
`primary_position`, `psr`, `osr`, `dsr`, `weighted_90s`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Latest PSR leaderboard
player_psr()

# As of a specific date
player_psr(date = "2026-03-18")

# Look up a specific player
player_psr(date = "2026-03-18", player = "Salah")

# Top midfielders
player_psr(position = "MID")
} # }
```
