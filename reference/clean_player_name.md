# Clean player name for matching

Creates a minimal normalized version of player name for fuzzy matching.
Unlike standardize_player_names() which preserves readable format, this
creates a key: lowercase with all whitespace removed. Uses memoization
to cache unique names for O(1) lookup on repeated values.

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
clean_player_name(c("Kylian Mbappé", "kylian mbappé", "KYLIAN MBAPPÉ"))
# All return "kylianmbappé"
} # }
```
