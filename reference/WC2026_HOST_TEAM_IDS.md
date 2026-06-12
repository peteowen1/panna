# Opta team_ids of the three WC 2026 hosts (USA / Canada / Mexico)

Keyed by team_id rather than name because Opta has already served at
least one name variant for these teams ("USA" vs "United States" – see
the fixture-name normalisation block in 01_build_fixture_results.R).
step 04 asserts all three IDs resolve in the WC2026 fixture set before
flagging host advantage.

## Usage

``` r
WC2026_HOST_TEAM_IDS
```

## Format

An object of class `character` of length 3.
