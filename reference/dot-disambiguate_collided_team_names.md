# Disambiguate team-name collisions before Elo iteration

[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md)
keys its internal state by team NAME, so two genuinely different clubs
that happen to share a name (confirmed: "Arsenal" is both the EPL club
and Arsenal de Sarandi in the Argentine Liga Profesional) blend their
match histories into one Elo trajectory – unlike the panna#204 case (one
real club, several name spellings), this is one real name, several real
clubs. Only usable when `results` carries `home_team_id`/`away_team_id`
(a no-op otherwise, so old callers/tests without those columns see zero
behavior change).

## Usage

``` r
.disambiguate_collided_team_names(results)
```

## Arguments

- results:

  As in
  [`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md).

## Value

`results` with `home_team`/`away_team` disambiguated where needed, plus
an `id_rename_map` attribute (named character vector, possibly length
0).

## Details

For each name shared by \>1 distinct `team_id`, the identity with the
most rows keeps the plain name (the common case); every other identity
sharing that name gets a `[id:xxxxxxxx]` suffix appended, so the Elo
simulation tracks them as separate teams. This only affects the internal
keys used during iteration and the names returned in `final_elos` –
per-match rows are returned aligned by `match_id`/row order regardless,
so this is transparent to any caller that only reads `per_match`. A
caller doing a post-hoc name lookup against `final_elos` for an UPCOMING
fixture (e.g. step 03's `lookup_elo()`) needs the same disambiguation
applied to its own lookup key – see the returned `id_rename_map` (named
character vector, `team_id -> disambiguated name`, empty if no
collisions were found).
