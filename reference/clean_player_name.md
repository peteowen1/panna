# Clean player name for matching

Creates a minimal normalized version of player name for fuzzy matching:
lowercase with all whitespace removed (a matching key, not a display
form). Uses memoization to cache unique names for O(1) lookup on
repeated values.

## Usage

``` r
clean_player_name(names)
```

## Arguments

- names:

  Character vector of player names

## Value

Character vector of cleaned names (lowercase, no whitespace)

## Examples

``` r
if (FALSE) { # \dontrun{
clean_player_name(c("Kylian Mbapp\u00e9", "kylian mbapp\u00e9", "KYLIAN MBAPP\u00c9"))
# All return "kylianmbapp\u00e9"
} # }
```
