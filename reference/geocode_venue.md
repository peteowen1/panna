# Geocode a Venue

Looks up latitude/longitude for a venue name using a built-in reference
table of major football stadiums. Falls back to Nominatim
(OpenStreetMap) for unknown venues.

## Usage

``` r
geocode_venue(venue, country = NULL, use_nominatim = TRUE)
```

## Arguments

- venue:

  Character. Venue/stadium name.

- country:

  Character. Country hint for disambiguation (optional).

- use_nominatim:

  Logical. If TRUE, tries Nominatim API for unknown venues.

## Value

Named list with `lat`, `lon`, `source` ("reference" or "nominatim")
