# Play one knockout round (internal helper)

Resolves all ties in a round from the integer-indexed knockout matrices,
applies the run-hot Elo nudge, simulates goals for the margin, and
updates both teams' dynamic Elo. Round-level randomness is drawn in
blocks. A 90-minute draw is decided by a coin flip (extra time /
penalties) but counts as a draw for the Elo update.

## Usage

``` r
play_knockout_round(bracket, WIN, DRAW, LAM, elo_dyn, elo_base, elo_k)
```
