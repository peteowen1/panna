# Per-(era, role) mean of each skill feature (the within-position baseline)

Position stat-profiles drift across eras, so means are computed PER
season-end-year x role (cells with \>= `min_n` player-matches), plus a
role-overall fallback row (`season_end_year = NA`) for thin/missing
cells. Scoring (`.position_normalize_skills`) looks up the
player-season's era, falling back to the role-overall mean — so both
current and historical game-logs get an era-appropriate baseline.

## Usage

``` r
compute_position_role_means(
  player_stats,
  skill_cols,
  min_n = 200L,
  role_grain = c("broad", "role8")
)
```

## Arguments

- player_stats:

  Player-level skill table with position, season (or season_end_year)
  and the skill feature columns.

- skill_cols:

  Skill feature names to summarise.

- min_n:

  Minimum player-matches for a per-(season, role) cell to be kept.

- role_grain:

  Role bucket to key on. `"broad"` (default) keys on GK/DEF/MID/FWD;
  `"role8"` keys on GK/CB/FB/DM/CM/AM/W/ST, which stops centre-backs
  being centred on a mean pooled with attacking full-backs and attacking
  midfielders on one pooled with holding midfielders. The grain written
  here is DETECTED at scoring time by `.position_means_grain()`; writing
  `"role8"` obliges every scoring caller to resolve a role at that grain
  (skills tables carry only the broad `primary_position`, so they must
  pass `.position_normalize_skills(role_override=)`) or normalization
  aborts.

## Value

data.table(season_end_year, role, stat_name, mean); rows with
`season_end_year = NA` are the role-overall fallback.
