# Every shooter's earlier foot shots, from the full local shot history

The weak-foot input counts a player's foot shots in EARLIER matches in
every league, so it is built once from the consolidated shot events and
fixtures (all leagues), not per league-season. Cached for the session.

## Usage

``` r
.load_shot_foot_history(refresh = FALSE)
```

## Arguments

- refresh:

  Rebuild even if cached.

## Value

[`.shot_foot_history()`](https://peteowen1.github.io/panna/reference/dot-shot_foot_history.md)
output.
