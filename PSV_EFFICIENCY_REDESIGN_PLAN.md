# PSV / PSR Efficiency Redesign — Implementation Plan

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

### 🔨 Remaining (the wiring + retrain)
1. **Loader** for per-match xmetrics (`xmetrics_bymatch/`) — mirror
   `load_opta_xmetrics()` / `.load_opta_xmetrics_data()` in `R/opta_loaders.R`.
2. **Real join** into `match_stats` replacing the stub at
   `02_estimate_skills.R:55` — join `xmetrics_bymatch` by `(player_id, match_id)`;
   add per-90 derivations (`ibox_g_minus_xg_per90`, etc.). This finally puts xG
   in the model.
3. **GK GSAA** aggregation (`xG_faced − goals_conceded`): attribute opponent
   shot xG to the keeper (new aggregation; uses existing per-shot xg / xGOT).
4. **Feature lists**: add over-performance cols to `.get_psr_skill_cols()` +
   `R/skill_config.R`; GSAA to `.get_gk_skill_cols()`. Remove the 6 finishing
   ratios from `efficiency_cols`; **sync the hardcoded `success_cols`** at
   `R/spm_opta.R:598` (duplicate of the efficiency list — desync trap).
5. **Blended target**: `07_train_psr_model.R` lines ~739–762 — single model on
   `α·xg_diff + (1−α)·goal_diff` (param `psv_target_alpha`, default 0.6);
   write coefficient CSVs.
6. **Retrain** (local, ~2–3h, 20–25 GB — OOMs CI): skills pipeline steps 01→07
   (`run_skills_pipeline.R`). Then regenerate game-logs (10b) + verify Messi on
   the retrained model.
7. Optional: α backtest before locking the default.

## Notes / gotchas
- `match_stats` carries `match_id` (join key OK).
- Over-performance features have low repeatability → PSR's skill-estimation will
  shrink them (correct: finishing regresses); PSV uses the raw per-match value
  (full hat-trick credit). Rating-vs-Production split, automatic.
- Don't flip `exclude_efficiency`/centering defaults globally — they move the
  RAPM `psvf90` target. Display-layer only, or stratify RAPM folds.
