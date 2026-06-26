# Canonical "rating/display" league set (shared across pipelines)

Single source of truth for the competitions the pipelines RATE and
DISPLAY, so the EPV/xMetrics (step 03), skills/PSR, RAPM/panna and blog
(10b) pipelines cannot silently drift apart (the 2026-06 audit found
four different lists). Grouped by season-label convention because 10b
resolves labels per group ("YYYY-YYYY" domestic vs calendar "YYYY" vs
tournament "YYYY Country"). `PANNA_RATING_LEAGUES` is the flat union (25
comps). Bridge comps live in `PANNA_BRIDGE_LEAGUES` (offset/RAPM
connectivity only, never displayed) and are added ON TOP in step 03 /
RAPM.

## Usage

``` r
PANNA_LEAGUE_GROUPS

PANNA_RATING_LEAGUES

PANNA_BRIDGE_LEAGUES
```

## Format

An object of class `list` of length 4.

An object of class `character` of length 25.

An object of class `character` of length 6.
