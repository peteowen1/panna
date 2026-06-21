# PSV / PSR Efficiency Redesign — Implementation Plan

> ✅ **SHIPPED 2026-06-21.** panna PR #107 merged to `main`; 11 seasons
> (2015-16 → 2025-26) rebuilt on the trained blend model and live on R2.
> Messi WC2026 hat-trick PSV **−0.37 → +0.18**; keepers route through the GK
> model (GSAA); finishing-aware PSV (attackers top the Value tab); new display
> stats `goals_minus_xgot`/`placement_added` (blog PR inthegame-blog#332).
> Pre-2015 seasons stay on the old model (no xGOT data exists that far back).
> The deep gotchas (`_per90` regex, step-7 xG join, GK routing, box-minute
> override, blend target) are now in `CLAUDE.md`. This doc is the historical
> record of how it was built.

Tracks the redesign triggered by **pannadata #79** (WC Value tab: per-90 PSV not
summable; Messi hat-trick reads negative PSV). Two root causes, both confirmed
against real data + coefficients:

1. **Train/serve skew** — efficiency stats (incl. `ibox_goal_rate`, β +0.112)
   are in the *trained* model but dropped at PSV scoring (`exclude_efficiency =
   TRUE`). PSR keeps them; PSV doesn't.
2. **Scale-free ratios** — a per-game ratio (`ibox_goal_rate = goals/shots`)
   gives 1/1 == 10/10, discarding volume. Should be **over-performance counts**
   (`goals − xG`).
3. **Latent gap discovered** — the "Enrich with xMetrics" step
   (`02_estimate_skills.R:55`) is a **stub**; xG never joins into `match_stats`,
   so the value model has had **no xG at all** (zero `xg_per90` coefficient
   rows). `opta_xmetrics.parquet` is per-**season** (no `match_id`) so it can't
   join to per-match `match_stats`.

## Decisions

- **PSV scaled to minutes** (additive like EPV); web divides for per-90. Opt-in
  `scale_to_minutes`, on only in the blog export (RAPM target `psvf90` stays
  per-90). ✅ done.
- **Use the model as trained** — `exclude_efficiency = FALSE` in the export +
  per-90 ratio-guard so ratios aren't wrongly divided by minutes. ✅ done.
- **Per-match xG** via opt-in `by_match` param on `aggregate_player_xmetrics()`
  (season-level default untouched → existing consumers safe). ✅ done.
- **Replace only the low-volume *finishing* ratios** with xG over-performance
  counts (`npg−npxg`, `ibox/obox_g−xg`); **keep** high-volume accuracy ratios
  (pass/duel/aerial — large denominators, glmnet shrinks redundancy).
- **GK**: `save_percentage → GSAA` (`xG_faced − goals_conceded`).
- **Blended target** for the displayed value model: `α·xg_diff + (1−α)·goal_diff`,
  α ≈ 0.6 (tunable), so finishing registers. Keep pure xG-diff for the RAPM
  target.

## Status

### ✅ Done & tested
| Change | File | Test |
|---|---|---|
| `scale_to_minutes` (additive PSV) | `R/psr.R` `calculate_psv*` | test-psv.R |
| `exclude_efficiency` threaded + per-90 ratio-guard | `R/psr.R`, `10b_export_game_logs.R` | test-psv.R |
| Zonal/overall over-performance features | `R/xg_model.R` `aggregate_player_xmetrics` | test-xg-model.R |
| `by_match` per-match aggregation | `R/xg_model.R` | test-xg-model.R (2-match) |
| Per-match xmetrics artifact emitted | `data-raw/epv/03_calculate_player_xmetrics.R` | (pipeline) |

Messi WC2026 hat-trick: PSV **−0.37 → +0.36** (Tier 1 alone).

### ✅ Wiring done (batch 2)
1. **Loader** — `load_opta_xmetrics(..., by_match = TRUE)` reads
   `xmetrics_bymatch/`. `R/opta_loaders.R`.
2. **Real join** — `02_estimate_skills.R` §5 now joins `xmetrics_bymatch` by
   `(player_id, match_id)`; per-90 over-performance derivations added in
   `aggregate_player_xmetrics`. Replaces the stub → **xG is in the model**.
4. **Feature lists** — over-performance cols added to `.get_psr_skill_cols()` +
   `R/skill_config.R`; 6 finishing ratios removed from both. (`spm_opta`
   `success_cols` left as-is: finishing ratios are still *computed*, only dropped
   from PSR/PSV features — SPM unaffected.)
5. **Blended target** — `07_train_psr_model.R` trains a 3rd `blend_` set on
   `α·xg_diff + (1−α)·goal_diff` (`psv_blend_alpha`, default 0.6). `target="blend"`
   wired through `load_psr_coefficients`/`compute_player_psv` (graceful fallback
   to xG until trained); export uses it.

### ✅ GK GSAA done (batch 3)
3. **GK GSAA** — `.compute_keeper_gsaa()` (cross-team: opponent shot xGOT →
   conceding team's keeper; expected goals faced − goals conceded). Flows through
   `aggregate_player_xmetrics`; `gsaa_per90` replaces `save_percentage` in
   `.get_gk_skill_cols()` + `skill_config.R` + the step-02 join. Unit-tested.

### 🔨 Remaining — the retrain only
6. **Retrain** (local, ~2–3 h, 20–25 GB — OOMs CI): **re-run
   `03_calculate_player_xmetrics.R` first** (generates `xmetrics_bymatch/` incl.
   gsaa), then skills pipeline steps 01→07. Regenerate game-logs (10b) + verify
   Messi (outfield) and a keeper on the retrained blend model.
7. Optional: α backtest before locking the default.

All code is in place; the retrain is the only remaining step to make it live.

## Notes / gotchas
- `match_stats` carries `match_id` (join key OK).
- Over-performance features have low repeatability → PSR's skill-estimation will
  shrink them (correct: finishing regresses); PSV uses the raw per-match value
  (full hat-trick credit). Rating-vs-Production split, automatic.
- Don't flip `exclude_efficiency`/centering defaults globally — they move the
  RAPM `psvf90` target. Display-layer only, or stratify RAPM folds.
