# Season totals of the net goals breakdown, one block of rows per player

The player page shows a season's EPV by play type for one player.
Reading the per-match breakdown for that took 13.7 s on a past season
(2.4M rows, 21 MB), because the page downloads every player's matches to
draw one. This sums each player's season across every competition they
played, and the file is written sorted by player in small row groups so
the page reads one group.

## Usage

``` r
.ng_breakdown_players(bd)
```

## Arguments

- bd:

  [`.ng_breakdown()`](https://peteowen1.github.io/panna/reference/dot-ng_breakdown.md)
  rows for one season: `match_id`, `player_id`, `category`, `value`.

## Value

data.table sorted by `bucket`
([`.ng_player_bucket()`](https://peteowen1.github.io/panna/reference/dot-ng_player_bucket.md))
then `player_id`: `bucket`, `player_id`, `category`, `value` (season
total, goals), `games` (matches with a breakdown) and `net_goals` (the
player's season total, the same on each of their rows, so the page can
check its parts add up).
