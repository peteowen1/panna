# Convert League Code to Opta Format

Converts panna league codes (ENG, ESP, etc.) to Opta format (EPL,
La_Liga, etc.). Matching is case-insensitive for convenience (e.g.,
"epl", "eng", "Eng" all work). Falls back to catalog lookup, then
pass-through for valid-looking codes.

## Usage

``` r
to_opta_league(league)
```

## Arguments

- league:

  Panna league code or Opta league code (case-insensitive).

## Value

Opta league code.
