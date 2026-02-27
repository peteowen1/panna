# Extract FBref player ID from href

Extracts the 8-character hex ID from an FBref player URL. Example:
"/players/d080ed5e/Kylian-Mbappe" -\> "d080ed5e"

## Usage

``` r
extract_fbref_player_id(hrefs)
```

## Arguments

- hrefs:

  Character vector of FBref player hrefs

## Value

Character vector of 8-char hex IDs (NA if not found)

## Details

Note: player_href is extracted during FBref scraping and used in data
processing to derive player IDs.

## Examples

``` r
if (FALSE) { # \dontrun{
extract_fbref_player_id("/players/d080ed5e/Kylian-Mbappe")
# Returns "d080ed5e"
} # }
```
