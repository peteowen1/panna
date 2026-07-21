# MODELS.md — which model is used where, and how to pin it

Single source of truth for every ML model in the panna value pipeline: the loader,
its fallback chain, the canonical (correct) version, and the override to pin it.

> ⚠ **The trap this file exists to prevent.** Every `load_*_model()` has a SILENT
> fallback chain: explicit `path` → `pannamodels` package → local
> `pannadata/data/opta/models/*.rds`. If the preferred source is absent (e.g.
> `pannamodels` not installed) it falls through with **no error** and can return a
> STALE model. This shipped inflated EPV on 2026-06-21 (a standalone game-logs
> rebuild used the pre-overhaul `epv_model.rds` instead of the post-overhaul clean
> model → Messi EPV 3.65 instead of 2.49, every player positive). The loaders now
> print the resolved file + **modification date** and warn if >14 days old
> (`.report_model_provenance` in `R/epv_model.R`) — but the real safety is to
> **pass explicit overrides for game-logs** (below).

## The models

| Model | Loader | Fallback chain | Default resolves to | Iterating? |
|-------|--------|----------------|---------------------|-----------|
| **EPV** | `load_epv_model()` | path → pannamodels → local | `pannadata/.../epv_model.rds` | **YES** — overhauled 2026-06-19 |
| **WP** | `load_wp_model()` | path → pannamodels → local | `pannadata/.../wp_model.rds` | **YES** — overhauled 2026-06-19 |
| **xG** | `load_xg_model()` | path → pannamodels → local | `pannadata/.../xg_model.rds` | stable |
| **xGOT** | `load_xgot_model()` | path → pannamodels → local → **NULL** | `pannadata/.../xgot_model.rds` | stable (optional; returns NULL if absent) |
| **xPass** | `load_xpass_model()` | path → pannamodels → local | `pannadata/.../xpass_model.rds` | stable |
| Minutes / Knockout | — | no preloaded model (heuristic / fit on demand) | — | — |

## Canonical (correct) models — the EPV/WP overhaul (2026-06-19)

The "Rice over-reactivity" overhaul produced **clean** EPV+WP models. Status of
the stale-default warning as of 2026-07-21 (verified by md5 during the explainer
baseline sweep): **EPV is healed** — `pannadata/.../epv_model.rds` is now
byte-identical to the canonical clean artifact, so the bare default is safe.
**WP is still stale** — the bare default remains the pre-overhaul model (old
feature set, no time interactions); the override below is still REQUIRED for WP.
Also note: the WP artifact's stored `cv_logloss` field is a misnomer — since the
`reg:squarederror` objective switch it actually holds **CV RMSE** (confirmed
against the training log); rename when the model is next retrained. Pin via
overrides:

```r
epv_model_override <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")  # 2026-06-19
wp_model_override  <- readRDS("data-raw/cache/epv/wp_final_d2repl_reg/wp_model.rds")  # 2026-06-19
```

This is exactly what `data-raw/match-predictions-opta/_run_gamelogs_gt.R` does —
**it is the canonical reference for building game-logs.** Any standalone game-logs
/ equity rebuild MUST set both overrides, or it ships the old over-reactive EPV/WP.

## Production call sites (where it matters)

| Script | Models | How it should load them |
|--------|--------|--------------------------|
| `10b_export_game_logs.R` (EPV/WPA/PSV) | EPV, WP, xPass | `epv_model_override` + `wp_model_override` set by the caller (else default fallback → STALE) |
| `10c_export_equity.R` (chains) | EPV, xPass | `epv_model_override` |
| `05_train_wp_model.R` | EPV (for the WP `epv` feature) | already passes explicit `path = "data-raw/cache/epv"` ✓ |
| `03_calculate_player_xmetrics.R` | xG | `load_xg_model()` (stable model, default OK) |

**Rule of thumb:** for anything that *exports* EPV/WPA to the blog, pass explicit
`epv_model_override`/`wp_model_override`. Never rely on the silent default for the
iterating (EPV/WP) models.

## How to rebuild game-logs correctly (the recipe)

```r
# from panna/
epv_model_override <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")
wp_model_override  <- readRDS("data-raw/cache/epv/wp_final_d2repl_reg/wp_model.rds")
blog_leagues       <- c(... blog leagues ...)
game_log_seasons   <- "2025-2026"   # or a vector for backfill
upload_game_logs   <- FALSE
build_game_logs    <- TRUE
use_skill_ratings  <- FALSE
source("data-raw/match-predictions-opta/10b_export_game_logs.R")
```

Watch the log for the provenance line — it must show the **clean** model is in
use (overrides bypass the loader, so you'll see the override taken, not a stale
`epv_model.rds` date).

## When models change

1. Train → write to `data-raw/cache/epv/` (the candidate).
2. Update the override paths in `_run_gamelogs_gt.R` AND this file.
3. Publish to pannamodels + `pannadata/data/opta/models/` so the *default* loader
   also returns the new version (closes the gap between override and default).
4. EPV feature-contract changes require the model + code lockstep — see the
   `EPV_SIMPLE_FEATURE_COLS` gotcha in `CLAUDE.md`.
