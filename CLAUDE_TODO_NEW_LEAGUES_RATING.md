# TODO — wire new leagues (MLS / Liga MX / Argentina / Saudi) into ratings

**Created 2026-06-05.** Companion to the backfill (pannadata; see memory
`project_new_leagues_backfill`). The 4 leagues are being **scraped** to
2013-14, but they are NOT yet **rated** — they’re absent from
`run_pipeline_opta.R`’s league set, so they don’t appear in
`ratings.parquet`, blog cards, game-logs, or the `league_strength`
table. This doc is the plan to integrate them. **No pipeline changes
yet** — gated on the backfill completing.

Panna codes to assign (code → Opta competition name in
`opta_player_stats`): `MLS → MLS`, `MEX → Liga_MX`,
`ARG → Argentine_Liga_Profesional`, `SAU → Saudi_League`.

------------------------------------------------------------------------

## The core challenge: cross-league comparability

RAPM/SPM/xRAPM (the `ratings.parquet` pipeline) use **no explicit league
fixed effect or offset** (confirmed: no league-FE in `spm_opta.R` /
`feature_engineering.R` / `panna_rating.R`). Cross-league scale emerges
ONLY from **shared-opponent connectivity** — teams/players that appear
against common opponents via: - **continental club cups** (the strongest
bridge), and - **international tournaments + qualifiers** (national-team
players link confederations).

A league with weak connectivity to the anchor pool (Big-5 + UCL) gets an
**unreliable cross-league scale** — its players’ ratings won’t be
comparable to EPL/UCL players even if internally consistent. So the real
work is **ensuring the bridge competitions are scraped**, not just
adding the 4 domestic leagues.

EPR (the EPV-based half, `player_ratings_epv.R` →
`calculate_epr_regression`) *does* use
[`compute_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md)
(anchor = UCL group stage) with a **chained** estimator for leagues
lacking a direct UCL bridge — so once these leagues have EPV `game_logs`
AND a chain path, EPR self-corrects their scale.

### Bridge competitions each new league needs (verify in entitlement catalog, scrape if missing)

| League        | Primary bridges to the anchor pool                                                                                       |
|---------------|--------------------------------------------------------------------------------------------------------------------------|
| **Argentina** | CONMEBOL Libertadores + Sudamericana → (Brazil already rated) → Club World Cup → UEFA                                    |
| **Liga MX**   | Concacaf Champions Cup + **Leagues Cup** (MLS↔︎Liga MX) → Club World Cup → UEFA                                           |
| **MLS**       | Leagues Cup (↔︎Liga MX) + Concacaf Champions Cup + Club World Cup → UEFA                                                  |
| **Saudi**     | AFC Champions League Elite → (Asian clubs) + Club World Cup → UEFA                                                       |
| all 4         | Internationals already scraped (WC/Copa/Gold Cup/Asian Cup + qualifiers) link national-team players — secondary but real |

`Club_World_Cup`, `CONMEBOL_Libertadores`, `AFC_Champions_League_Elite`
are already configured with correct IDs (2026-06-05 fix). The remaining
bridges exist in our entitlement (verified 2026-06-05) but are **NOT yet
in `COMPETITIONS`** — add them (with these confirmed IDs) and scrape
their history, as they’re the load-bearing club bridges:

| Add to COMPETITIONS      | Opta ID                     | Bridges                               |
|--------------------------|-----------------------------|---------------------------------------|
| `Leagues_Cup`            | `9fuwphq8kvugrlc3ckm7k8wes` | **MLS ↔︎ Liga MX** (critical)          |
| `Concacaf_Champions_Cup` | `e6rl4hongahbihxd3tpudespd` | MLS/Liga MX ↔︎ Concacaf                |
| `CONMEBOL_Sudamericana`  | `32n2r9bl6x90psj0wa7bfs6vq` | Argentina/Brazil 2nd-tier continental |
| `CONMEBOL_Recopa`        | `7xq2oxd5qkygl6o5xymxwhoxe` | (minor) champions playoff             |

Without `Leagues_Cup` + `Concacaf_Champions_Cup` scraped, MLS and Liga
MX will have almost no direct bridge to the anchor pool and their
cross-league scale will be unreliable — scrape these FIRST among the
bridges.

------------------------------------------------------------------------

## Step-by-step (in order; each gated on the prior)

1.  **Backfill complete** (pannadata) — all 4 leagues + the bridge comps
    above scraped to 2013-14 and on `opta-latest`. Re-scrape Brazil too
    (calendar-fix recovers its missing Aug-Dec).
2.  **League codes** — add `MLS/MEX/ARG/SAU` (+ any new bridge comps) to
    the code→name map in `R/opta_loaders.R` (the `LEAGUE_NAMES`-style
    table, ~L80-110). Confirm `load_opta_stats("MLS")` resolves.
3.  **Add to rating set** — append the 4 codes (+ bridges) to `leagues`
    in `data-raw/player-ratings-opta/run_pipeline_opta.R` (currently
    20).
4.  **Full RAPM/SPM/xRAPM rerun** — `force_rebuild_from <- 1` (or rerun
    the `opta-pipeline.yml`). RAPM is cross-season/cross-league by
    design; the new leagues fold in automatically IF connectivity
    exists. Heavy (~the full pipeline).
5.  **Connectivity sanity check** — after step 4, verify the new
    leagues’ players land on a plausible scale (e.g., a known MLS↔︎Europe
    transfer keeps a sane rating delta; an MLS star isn’t rated above
    prime-EPL). If a league looks mis-scaled, its bridges are too thin →
    scrape more bridge-comp history.
6.  **EPR offsets** — run `debug/keep/build_league_offsets.R`; confirm
    the new leagues get a `chained` offset (check `n_obs` is
    non-trivial). May need to add their codes to `chain_intermediates` /
    ensure a bridge league is in the chain set. Then
    `build_epr_weekly.R` picks them up.
7.  **Skills pipeline** — add the 4 codes to `leagues` in
    `data-raw/estimated-skills/run_skills_pipeline.R` +
    `01_compute_match_stats.R` (keep them OUT of the TUN/CAF-style blog
    trim — these ARE wanted as cards), rerun steps 01-08 →
    `opta_skills.parquet` gains their box-score cards.
8.  **Blog game-logs** — add the 4 codes to `blog_leagues`
    (`domestic_leagues`) in `10b_export_game_logs.R`,
    `10c_export_equity.R`, `10d_export_shootout_wpa.R` so per-match
    EPV/WPA/PSV export for them.
9.  **Refresh blog data** — predictions run → `build-blog-data.yml` →
    R2. New-league players now have ratings + cards + value tabs.
10. **`league_strength`** — auto-extends once step 6/8 give them EPV
    game_logs; re-run `build_league_strength.R`.

------------------------------------------------------------------------

## Risks / watch-fors

- **Thin connectivity = unreliable scale** (the \#1 risk). Bridges
  (esp. Leagues Cup, Libertadores, AFC CL) must have enough scraped
  history. Sanity-check (step 5).
- **Saudi recency** — its talent influx is post-2023; pre-2023 Saudi
  scale won’t reflect current squads. Recency-weighting in EPR helps;
  RAPM is season-agnostic.
- **Calendar-year leagues** already handled (date-window fix
  2026-06-05). Their season labels are single-year (`2025`), not
  `2025-2026` —
  [`match_is_international()`](https://peteowen1.github.io/panna/reference/match_is_international.md)
  and any season-string logic must treat them as club/domestic (they’re
  in `MATCH_CLUB_LEAGUES` already for BEL/BRA/AUS/TUN; add the 4 new
  codes there too — `R/constants.R`).
- **Replacement-level / min-minutes filters** apply as usual at export.

## Effort

Steps 2-3, 7-8 are small edits. Steps 4 (RAPM rerun) + 6 are the compute
cost. Step 1 (backfill incl. bridges) is the long pole. Realistic: a few
days once the backfill lands, dominated by reruns + connectivity
validation.
