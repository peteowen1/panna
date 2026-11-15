# 05d_train_wp_structural.R
# Train the structural win-probability model WPA uses (panna#273): SEP + starting-XI strength.
#
#   Grain   one row per on-ball SPADL action (1 in TRAIN_EVERY per match for training).
#   Target  the match result from the home side: 1 / 0.5 / 0.
#   Inputs  time remaining, home score margin, red-card difference, second half, extra time,
#           pre-match Elo gap (home - away, every league), who has the ball (SEP),
#           xi_diff (starting-XI strength from as-of career Panna, leak-free).
#           Live threat enters after the model as the next-goal mixture (predict_wp_structural()).
#   Train   seasons ending TRAIN_END_YEARS; test: seasons ending TEST_END_YEAR, all rows.
#   Gate    the test log loss must beat the published possession-frame WP model, or the script stops.
#
# Writes to WP_STRUCT_DIR only (default data-raw/cache/epv/wp_structural/): the model rds, a
# metadata csv, per-competition test scores. It never uploads: publishing is a separate, approved step.
# Decided with Pete 2026-11-10 (pannaverse DECISIONS); plan: pannaverse docs/plans/WPA-LEDGER-273.md.
#
# Run from panna/ under PowerShell (arrow):  Rscript data-raw/epv/05d_train_wp_structural.R
# Smoke run: set wp_struct_leagues <- c("ENG", "NED") before sourcing.

library(data.table)
devtools::load_all()

# 1. Configuration ----
WP_STRUCT_DIR   <- if (exists("wp_struct_dir")) wp_struct_dir else file.path("data-raw", "cache", "epv", "wp_structural")
FEATS_DIR       <- if (exists("wp_struct_feats_dir")) wp_struct_feats_dir else file.path(WP_STRUCT_DIR, "feats")
TRAIN_END_YEARS <- if (exists("train_end_years")) train_end_years else 2022:2025
TEST_END_YEAR   <- if (exists("test_end_year")) test_end_year else 2026L
TRAIN_EVERY     <- if (exists("train_every")) train_every else 8L
LEAGUE_FILTER   <- if (exists("wp_struct_leagues")) wp_struct_leagues else NULL
SPADL_DIR       <- file.path("data-raw", "cache", "epv", "spadl")
source("data-raw/epv/wp_structural_elo.R")   # WP_ELO_*_LEAGUES, wp_structural_match_elo(): shared with 10b
for (d in c(WP_STRUCT_DIR, FEATS_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
say <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))
say("05d: train seasons ending %s, test %d, 1 row in %d; output %s",
    paste(range(TRAIN_END_YEARS), collapse = "-"), TEST_END_YEAR, TRAIN_EVERY, WP_STRUCT_DIR)

# 2. Pre-match Elo for every league ----
elo_path <- file.path(WP_STRUCT_DIR, "match_elo.parquet")
elo <- wp_structural_match_elo(WP_STRUCT_DIR, rebuild = FALSE)

# 3. As-of career Panna (leak-free XI strength) ----
cpa_path <- file.path(opta_data_dir(), "career_panna_asof.parquet")
if (!file.exists(cpa_path)) cli::cli_abort("Missing {.path {cpa_path}}: run pb_download_opta() or career-panna-asof.yml.")
cpa <- as.data.table(arrow::read_parquet(cpa_path, col_select = c("player_id", "ref_date", "panna")))
say("career Panna as-of: %d rows, %d snapshots, %s to %s", nrow(cpa), uniqueN(cpa$ref_date), min(cpa$ref_date), max(cpa$ref_date))

# 4. Units and features (cached per league-season) ----
units <- data.table(f = list.files(SPADL_DIR, pattern = "^spadl_.*\\.rds$"))
units[, key := sub("^spadl_(.*)\\.rds$", "\\1", f)]
units[, league := sub("_[^_]*$", "", key)][, season := sub("^.*_", "", key)]     # league names can hold spaces
units[, end_year := extract_season_end_year(season)]
units[, role := fcase(end_year %in% TRAIN_END_YEARS, "train", end_year == TEST_END_YEAR, "test", default = NA_character_)]
units <- units[!is.na(role)]
if (!is.null(LEAGUE_FILTER)) units <- units[league %in% LEAGUE_FILTER]
if (!nrow(units)) cli::cli_abort("No league-seasons in {.path {SPADL_DIR}} for the requested years.")
say("units: %d train, %d test", units[role == "train", .N], units[role == "test", .N])

# Models are PINNED to the live set in MODELS.md, never the bare loaders: without the pannamodels package
# they fall back silently to old local copies (2026-11-10: a 2026-07-16 WP and a 2026-06-21 EPV).
.pin <- function(var, path, md5) {
  if (exists(var)) return(get(var))
  if (!file.exists(path)) cli::cli_abort("Missing {.path {path}}; set {.code {var}} (see MODELS.md).")
  got <- unname(tools::md5sum(path))
  if (!startsWith(got, md5)) cli::cli_abort("{.path {path}} md5 {substr(got, 1, 8)}, expected {md5} (MODELS.md).")
  say("model %s: %s (md5 %s)", var, path, md5); readRDS(path)
}
wm <- .pin("wp_model_override", "data-raw/cache/epv/pack-2026-09/wp_v51/wp_model.rds", "1e57310d")   # the published WP: the gate's comparison
keep <- unique(c("match_id", "action_id", "team_id", "period_id", "time_seconds", "score_diff", "is_home",
                 "red_card_diff", "time_remaining", "is_second_half", "is_extra_time", "epv", "wp_label",
                 wm$feature_names))
feat_path <- function(u) file.path(FEATS_DIR, sprintf("wpfeat_%s.parquet", u$key))
# The live EPV v5.1 + xG v5.2 pair 10b serves with (MODELS.md). xG v5.2 needs the events and each
# shooter's earlier foot shots, as in 05_train_wp_model.R.
epv_model <- .pin("epv_model_override", "data-raw/cache/epv/pack-2026-09/epv_model_v51v0.rds", "739407d2")
xg_model  <- .pin("xg_model_override", "data-raw/cache/epv/xg-vnext/xg_model_v5_2.rds", "a98df5d1")
foot_history <- if (.needs_shot_context(xg_model)) .load_shot_foot_history()
built <- character(0); failed <- character(0)
for (i in seq_len(nrow(units))) {
  u <- units[i]; fp <- feat_path(u); if (file.exists(fp)) next
  res <- tryCatch({
    events <- load_opta_match_events(u$league, season = u$season, source = "local")
    lineups <- load_opta_lineups(u$league, season = u$season, source = "local")
    spadl <- readRDS(file.path(SPADL_DIR, u$f)); spadl <- spadl[spadl$match_id %in% unique(events$match_id), ]
    ch <- create_possession_chains(spadl)
    ch <- calculate_action_epv(ch, features = NULL, epv_model, xg_model = xg_model, league = u$league, season = u$season,
                               shot_lookup = .epv_shot_lookup(u$league, u$season), events = events,
                               foot_history = foot_history)
    ch <- add_red_card_to_chains(ch, events)
    wf <- as.data.table(create_wp_features(ch, .build_match_results_from_events(events, lineups)))[!is.na(wp_label)]
    wf <- wf[, intersect(keep, names(wf)), with = FALSE]
    if (u$role == "train") { setorder(wf, match_id, period_id, time_seconds); wf <- wf[, .SD[seq(1L, .N, by = TRAIN_EVERY)], by = match_id] }
    arrow::write_parquet(wf, fp); "ok"
  }, error = function(e) conditionMessage(e))
  if (identical(res, "ok")) built <- c(built, u$key) else { failed <- c(failed, u$key); say("FAILED %s: %s", u$key, res) }
  suppressWarnings(rm(events, lineups, spadl, ch, wf)); gc(verbose = FALSE)
}
say("features: %d built this run, %d failed", length(built), length(failed))

# 5. Join Elo + XI strength; coverage is reported per league-season, never filled ----
cov <- list()
rd <- function(u) {
  fp <- feat_path(u); if (!file.exists(fp)) return(NULL)
  d <- as.data.table(arrow::read_parquet(fp)); n_m <- uniqueN(d$match_id)
  d <- suppressMessages(add_wp_match_elo(d, elo, min_coverage = 0, drop_missing = TRUE))
  lu <- tryCatch(load_opta_lineups(u$league, season = u$season, source = "local"), error = function(e) NULL)
  if (is.null(lu)) { cov[[u$key]] <<- data.table(unit = u$key, role = u$role, matches = n_m, elo = 0, xi = 0); return(NULL) }
  d <- suppressMessages(add_wp_xi_strength(d, lu, cpa))
  m <- d[, .(xi = !is.na(xi_diff[1])), by = match_id]
  cov[[u$key]] <<- data.table(unit = u$key, role = u$role, matches = n_m, elo = uniqueN(d$match_id) / n_m, xi = mean(m$xi))
  if (uniqueN(d$match_id) / n_m < 0.9) return(NULL)
  d[, unit := u$key]
}
tr <- rbindlist(lapply(split(units[role == "train"], seq_len(units[role == "train", .N])), rd), fill = TRUE)
if (!nrow(tr)) cli::cli_abort("No training rows after the Elo join.")
say("train: %d rows, %d matches, %d league-seasons; xi_diff present on %.1f%% of matches", nrow(tr),
    uniqueN(tr$match_id), uniqueN(tr$unit), 100 * tr[, .(x = !is.na(xi_diff[1])), by = match_id][, mean(x)])

# 6. Fit ----
fit <- train_wp_structural(tr, use_possession = TRUE, extra_features = "xi_diff")
rm(tr); gc(verbose = FALSE)

# 7. Test: every test league-season, all rows, against the published model ----
ll <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -(y * log(p) + (1 - y) * log(1 - p)) }
acc <- list()
for (i in which(units$role == "test")) {
  d <- rd(units[i]); if (is.null(d)) next
  setorder(d, match_id, period_id, time_seconds)
  for (nm in c("published", "structural")) {
    p <- if (nm == "published") predict_wp(wm, d) else predict_wp(fit, d)
    x <- copy(d)[, p := p][, `:=`(pn = shift(p, type = "lead"), tn = shift(team_id, type = "lead"),
                                  sdn = shift(score_diff, type = "lead")), by = match_id]
    x[, kind := fcase(is.na(tn), NA_character_, sdn != score_diff, "goal", tn == team_id, "same", default = "turnover")]
    x[, sw := abs(fifelse(tn == team_id, pn - p, (1 - pn) - p))]
    acc[[paste(i, nm)]] <- data.table(unit = units$key[i], model = nm, rows = nrow(d), matches = uniqueN(d$match_id),
                                      ll = sum(ll(p, d$wp_label)), g_sw = x[kind == "goal", sum(sw)], g_n = x[kind == "goal", .N],
                                      t_sw = x[kind == "turnover", sum(sw)], t_n = x[kind == "turnover", .N])
  }
}
A <- rbindlist(acc); C <- rbindlist(cov)
if (!nrow(A)) cli::cli_abort("No test rows: nothing to gate on.")
tot <- A[, .(rows = sum(rows), matches = sum(matches), logloss = sum(ll) / sum(rows),
             goal_per_turnover = (sum(g_sw) / sum(g_n)) / (sum(t_sw) / sum(t_n))), by = model]
print(C[elo < 0.9 | xi < 0.8])
cat("\n=== Test (seasons ending ", TEST_END_YEAR, "): log loss lower is better; goal_per_turnover EPV ~35 ===\n", sep = "")
print(tot[, .(model, rows, matches, logloss = round(logloss, 5), goal_per_turnover = round(goal_per_turnover, 1))])
gain <- tot[model == "published", logloss] - tot[model == "structural", logloss]
fwrite(A, file.path(WP_STRUCT_DIR, "test_by_competition.csv")); fwrite(C, file.path(WP_STRUCT_DIR, "coverage.csv"))
if (!(gain > 0)) cli::cli_abort("Structural model does not beat the published one on the test seasons (gain {round(gain, 5)}); not saved.")

# 8. Save (local only) ----
fit$metadata <- list(trained_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"), train_end_years = TRAIN_END_YEARS,
                     test_end_year = TEST_END_YEAR, train_every = TRAIN_EVERY,
                     test_logloss = tot[model == "structural", logloss], published_logloss = tot[model == "published", logloss],
                     test_matches = tot[model == "structural", matches],
                     elo_md5 = unname(tools::md5sum(elo_path)), career_panna_asof_md5 = unname(tools::md5sum(cpa_path)),
                     career_panna_asof_max_ref_date = as.character(max(cpa$ref_date)))
saveRDS(fit, file.path(WP_STRUCT_DIR, "wp_structural_model.rds"))
fwrite(as.data.table(lapply(fit$metadata, function(v) paste(v, collapse = ","))), file.path(WP_STRUCT_DIR, "wp_structural_model_meta.csv"))
say("saved %s (test log loss %.5f vs published %.5f). NOT uploaded.", file.path(WP_STRUCT_DIR, "wp_structural_model.rds"),
    fit$metadata$test_logloss, fit$metadata$published_logloss)
