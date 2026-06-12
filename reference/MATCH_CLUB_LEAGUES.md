# Club (domestic) competitions

Competitions played between club teams. Used to split the
match-prediction models into a domestic (club) model and an
international (national-team) model – the two behave very differently
(international prediction leans on Elo + recent form; club prediction
leans on squad player-ratings). Any competition NOT in this list is
treated as international.

## Usage

``` r
MATCH_CLUB_LEAGUES
```

## Format

Character vector of panna short codes (matches what flows through
fixture_results\$league in step 01).

## Details

BUG-FIX 2026-05-28: previously this list contained "EPL" (the Opta-side
competition name) but the rest as panna short codes ("ESP", "ITA", ...).
Since the predictions pipeline passes SHORT CODES through (its `leagues`
vector is "ENG", "ESP", ...), match_is_international("ENG") was
returning TRUE – i.e., the entire English Premier League was being
trained on the international-specialist model and receiving the
international prediction blend. Replaced "EPL" with "ENG" and added
BEL/BRA/AUS/TUN/CAFCL so any future addition of those leagues to the
default set classifies correctly.
