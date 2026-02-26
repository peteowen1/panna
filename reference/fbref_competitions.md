# FBref competition metadata

A data frame containing FBref competition IDs and metadata for all
supported competitions.

## Usage

``` r
fbref_competitions
```

## Format

A data frame with columns:

- code:

  Short code used in file paths (e.g., "ENG", "WC")

- fbref_id:

  FBref competition ID for URL construction

- name:

  Full competition name

- url_slug:

  URL-friendly name for FBref URLs (e.g., "Premier-League")

- type:

  Competition type: "league", "cup", "european", "national_team"

- season_format:

  How seasons are formatted: "YYYY-YYYY" or "YYYY"

## Examples

``` r
fbref_competitions
#>               code fbref_id                       name              url_slug
#> 1              ENG        9             Premier League        Premier-League
#> 2              ESP       12                    La Liga               La-Liga
#> 3              GER       20                 Bundesliga            Bundesliga
#> 4              ITA       11                    Serie A               Serie-A
#> 5              FRA       13                    Ligue 1               Ligue-1
#> 6              UCL        8      UEFA Champions League      Champions-League
#> 7              UEL       19         UEFA Europa League         Europa-League
#> 8           FA_CUP      514                     FA Cup                FA-Cup
#> 9          EFL_CUP      690                    EFL Cup               EFL-Cup
#> 10    COPA_DEL_REY      569               Copa del Rey          Copa-del-Rey
#> 11       DFB_POKAL      521                  DFB-Pokal             DFB-Pokal
#> 12    COPPA_ITALIA      529               Coppa Italia          Coppa-Italia
#> 13 COUPE_DE_FRANCE      518            Coupe de France       Coupe-de-France
#> 14              WC        1             FIFA World Cup             World-Cup
#> 15            EURO      676 UEFA European Championship European-Championship
#> 16    COPA_AMERICA      685               Copa America          Copa-America
#> 17           AFCON      656      Africa Cup of Nations Africa-Cup-of-Nations
#> 18  NATIONS_LEAGUE      677        UEFA Nations League        Nations-League
#> 19        GOLD_CUP      681          CONCACAF Gold Cup              Gold-Cup
#> 20       ASIAN_CUP      664              AFC Asian Cup             Asian-Cup
#>             type season_format
#> 1         league     YYYY-YYYY
#> 2         league     YYYY-YYYY
#> 3         league     YYYY-YYYY
#> 4         league     YYYY-YYYY
#> 5         league     YYYY-YYYY
#> 6       european     YYYY-YYYY
#> 7       european     YYYY-YYYY
#> 8            cup     YYYY-YYYY
#> 9            cup     YYYY-YYYY
#> 10           cup     YYYY-YYYY
#> 11           cup     YYYY-YYYY
#> 12           cup     YYYY-YYYY
#> 13           cup     YYYY-YYYY
#> 14 national_team          YYYY
#> 15 national_team          YYYY
#> 16 national_team          YYYY
#> 17 national_team          YYYY
#> 18 national_team     YYYY-YYYY
#> 19 national_team          YYYY
#> 20 national_team          YYYY
fbref_competitions[fbref_competitions$type == "national_team", ]
#>              code fbref_id                       name              url_slug
#> 14             WC        1             FIFA World Cup             World-Cup
#> 15           EURO      676 UEFA European Championship European-Championship
#> 16   COPA_AMERICA      685               Copa America          Copa-America
#> 17          AFCON      656      Africa Cup of Nations Africa-Cup-of-Nations
#> 18 NATIONS_LEAGUE      677        UEFA Nations League        Nations-League
#> 19       GOLD_CUP      681          CONCACAF Gold Cup              Gold-Cup
#> 20      ASIAN_CUP      664              AFC Asian Cup             Asian-Cup
#>             type season_format
#> 14 national_team          YYYY
#> 15 national_team          YYYY
#> 16 national_team          YYYY
#> 17 national_team          YYYY
#> 18 national_team     YYYY-YYYY
#> 19 national_team          YYYY
#> 20 national_team          YYYY
```
