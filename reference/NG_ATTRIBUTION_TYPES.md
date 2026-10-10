# Opta types that are dropped from SPADL but still name a player who caused a possession change

[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
removes `OPTA_NON_GAMEPLAY_TYPES` before
[`calculate_action_epv()`](https://peteowen1.github.io/panna/reference/calculate_action_epv.md)
computes its `shift(..., type = "lead")`, so the ledger's idea of "who
was next" steps over every dropped row. Measured on ENG 2024-2025:
92,808 of 639,507 events dropped, **60,535 of them sitting exactly on a
possession change** (160.6 a match). Torp paid for the same defect –
adjacency computed after filtering silently rewrote who did what while
every conservation assertion stayed green.

## Usage

``` r
NG_ATTRIBUTION_TYPES
```

## Format

An object of class `integer` of length 9.

## Details

These nine types are the ones worth seeing. Each names a player, and
each sits on a possession change often enough to matter. Everything else
that is filtered (deleted events, substitutions, cards, period markers,
formation changes) is a marker with no attribution content and stays
invisible.

Counts are ENG 2024-2025, "on change" = the surviving rows either side
belong to different teams. **The table below is the named-player
subset**: its "on change" column sums to 43,241 of the 60,535, the
remaining 17,294 being markers (deleted events, substitutions, cards,
period boundaries) that also sit on a possession change but name nobody
worth paying.

|     |                  |        |           |
|-----|------------------|--------|-----------|
| id  | name             | n      | on change |
| 5   | Ball Out         | 35,860 | 30,934    |
| 6   | Corner Awarded   | 7,766  | 6,328     |
| 74  | Blocked Pass     | 5,421  | 1,844     |
| 2   | Offside Pass     | 1,264  | 1,027     |
| 55  | Offside Provoked | 1,264  | 1,020     |
| 45  | Challenge        | 5,908  | 869       |
| 56  | Shield Ball Opp  | 462    | 431       |
| 51  | Error            | 634    | 413       |
| 59  | Keeper Sweeper   | 463    | 375       |
