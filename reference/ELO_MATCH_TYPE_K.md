# Match-type K Lookup

Maps a league code to a base Elo K-factor.

## Usage

``` r
ELO_MATCH_TYPE_K
```

## Format

Named numeric vector: league code -\> base K.

## Details

Values OPTIMIZED via DEoptim v6 2026-05-29 using 3-fold CV with 3-way
logloss + Davidson draw model + venue factor + tunable decay. v6 = v5
retrained on the expanded intl corpus (post-2026-05-29 scrapes: AFCON
2023 Cote d'Ivoire, AFC/CAF/CONMEBOL WCQ historical cycles 2002-2014,
etc., ~4,000 intl matches total). Decay halflife converged at 7000 days
(essentially "off" – recent matches don't need extra weight; the data
set itself has enough recent signal). Best CV-mean logloss = 0.9782 (vs
v4 seed 1.0135, -3.49%).

Pre-2026-05-29 values (in this file: WC=80, continental=80,
qualifier=25, friendly=5). v6 dropped WC + continental K substantially
(80 -\> 44 / 50) and raised qualifier (25 -\> 59) – with WC 2022 + the
extra qualifier cycles in the training set, individual matches need to
move Elos less because the prior is better-anchored. Note: the 94/110/55
numbers that appeared in some v3/v4 intermediate DEoptim seed comments
are from optimizer trial points, not from the production constants ever
shipped in this file.
