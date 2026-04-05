# 10c_export_equity.R
# Export per-action EPV credit (equity) for the blog match-events page
#
# Produces action_equity.parquet: a slim lookup table with one row per
# SPADL action, keyed by match_id + original_event_id. The pannadata
# chain builder joins this onto chain parquets as the `equity` column.
#
# Current season only to keep file size manageable.

# 1. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
repo <- "peteowen1/pannadata"
tag <- "blog-latest"

blog_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA", "NED", "POR", "SCO", "TUR", "ENG2")

if (!exists("game_log_season")) game_log_season <- "2025-2026"

output_path <- file.path(cache_dir, "action_equity.parquet")

# 2. Load models ----

message("\n=== Building Action Equity ===\n")

epv_model <- load_epv_model()
xpass_model <- load_xpass_model()

# 3. Process each league ----

all_equity <- list()

for (league in blog_leagues) {
  tryCatch({
    message(sprintf("  Processing %s %s...", league, game_log_season))

    events <- load_opta_match_events(league, season = game_log_season)

    if (is.null(events) || nrow(events) < 100) {
      message(sprintf("    Skipping %s — insufficient data", league))
      next
    }

    n_matches <- length(unique(events$match_id))

    # SPADL conversion (now includes original_event_id)
    spadl <- convert_opta_to_spadl(events)
    spadl_chains <- create_possession_chains(spadl)
    chain_outcomes <- classify_chain_outcomes(spadl_chains)
    chain_outcomes <- add_next_chain_outcome(chain_outcomes)
    spadl_labeled <- label_actions_with_outcomes(spadl_chains, chain_outcomes)
    spadl_labeled <- create_next_goal_labels(spadl_labeled)

    # EPV credit assignment
    epv_features <- create_epv_features(spadl_labeled, n_prev = 3)
    spadl_epv <- calculate_action_epv(spadl_labeled, epv_features, epv_model)
    spadl_credit <- assign_epv_credit(spadl_epv, xpass_model)

    # Extract slim equity lookup: match_id + original_event_id + player_credit
    dt <- data.table::as.data.table(spadl_credit)
    equity <- dt[, .(
      match_id = match_id,
      event_id = original_event_id,
      equity = round(player_credit, 4)
    )]
    # Drop rows where event_id is NA (synthetic SPADL actions)
    equity <- equity[!is.na(event_id) & event_id != ""]

    all_equity[[league]] <- equity
    message(sprintf("    %d matches, %d actions with equity", n_matches, nrow(equity)))

    rm(events, spadl, spadl_chains, chain_outcomes, spadl_labeled,
       epv_features, spadl_epv, spadl_credit, dt, equity)
    gc(verbose = FALSE)

  }, error = function(e) {
    if (!grepl("not found|No data|does not exist", e$message)) {
      message(sprintf("    ERROR %s: %s", league, e$message))
    } else {
      message(sprintf("    Skipping %s — data not available", league))
    }
  })
}

# 4. Combine and write ----

if (length(all_equity) == 0) {
  stop("No equity data produced. Check that events are available for the current season.")
}

action_equity <- data.table::rbindlist(all_equity)
message(sprintf("\n  Combined: %d actions across %d leagues",
                nrow(action_equity), length(all_equity)))

arrow::write_parquet(action_equity, output_path)
message(sprintf("  Written: %s (%.1f MB)", output_path,
                file.size(output_path) / (1024 * 1024)))

# 5. Upload to GitHub Releases ----

message("\n=== Uploading equity to GitHub ===\n")

gh_check <- tryCatch(
  system2("gh", "--version", stdout = TRUE, stderr = TRUE),
  error = function(e) NULL
)
if (is.null(gh_check)) {
  stop("'gh' CLI is not installed or not on PATH.")
}

message(sprintf("  Uploading to %s/%s...", repo, tag))
result <- system2(
  "gh", c("release", "upload", tag, shQuote(output_path),
          "--repo", repo, "--clobber"),
  stdout = TRUE, stderr = TRUE
)
if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
  stop(sprintf("Failed to upload action_equity.parquet: %s", paste(result, collapse = "\n")))
}

# 6. Summary ----

message("\n========================================")
message("Action equity exported successfully!")
message("========================================")
message(sprintf("  %d actions across %d leagues", nrow(action_equity), length(all_equity)))
message(sprintf("  Season: %s", game_log_season))
message(sprintf("  Release: https://github.com/%s/releases/tag/%s", repo, tag))
