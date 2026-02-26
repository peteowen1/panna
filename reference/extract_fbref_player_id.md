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

Note: This function is ready for use when player_href is available in
scraped data. Currently player_href is not extracted during scraping.

## Examples

``` r
if (FALSE) { # \dontrun{
extract_fbref_player_id("/players/d080ed5e/Kylian-Mbappe")
# Returns "d080ed5e"
} # }
```
