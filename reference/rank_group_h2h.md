# Rank one group with FIFA tiebreakers (internal)

Orders the four teams of a group by points, goal difference, goals for,
then – for teams still tied on all three – by head-to-head points, GD
and GF among the tied teams, then by `tbk` (random, standing in for fair
play and drawing of lots).

## Usage

``` r
rank_group_h2h(p, d, f, tbk, m_a, m_b, g_a, g_b)
```

## Arguments

- p, d, f:

  Length-4 integer vectors: points, goal difference, goals for (aligned
  with local team slots 1..4).

- tbk:

  Length-4 numeric tiebreak randoms.

- m_a, m_b:

  Length-6 integer vectors: local team slots per group match.

- g_a, g_b:

  Length-6 integer vectors: final goals per group match.

## Value

Integer permutation of 1:4 (best first).
