#!/usr/bin/env Rscript
# Export the xPass (pass-completion probability) model
# (panna/R/xpass_model.R, data-raw/cache/epv/xpass_model.rds) to JSON for the
# Cloudflare Worker's live xpass_overperformance_per90 scoring.
#
# Wave-1 task A4 of pannaverse/docs/plans/LIVE-PSV-UNBLOCK-2026-07-20.md
# (design decision D2, correction #4 — the xPass NON-goal in the blog plan is
# overridden by Pete: we ship this export). Unlike the older 05b/05c/06
# export scripts (xgb.dump + digits=10, which predates D2 and can corrupt
# split thresholds), this script embeds the EXACT learner JSON produced by
# xgb.save.raw(raw_format = "json") verbatim — NEVER xgb.dump, no re-rounding
# of tree internals. Envelope numerics are written digits = 17 (sufficient
# to round-trip an IEEE-754 double exactly; verified empirically below).
#
# --- Aggregation to the xmetric the worker must reproduce ---
# Verified against R/xg_model.R::aggregate_player_xmetrics() (~line 831-838)
# and R/player_stats_opta.R (~line 534-535). Per player, per match:
#   sum_xpass              = sum(predict_xpass(pass_i)) over every pass the
#                             player ATTEMPTED (success or fail)
#   passes_completed       = count(passes where result == "success")
#   xpass_overperformance  = passes_completed - sum_xpass
#   xpass_avg              = sum_xpass / passes_attempted
#   xpass_overperformance_per90 = xpass_overperformance * 90 / minutes
# i.e. actual completions MINUS the sum of the model's predicted completion
# probabilities across every attempted pass — not an average, not a per-pass
# residual. The live worker must accumulate sum_xpass and passes_completed
# per player across the match and subtract, exactly like this.
#
# Run from panna/:
#   Rscript data-raw/epv/06d_export_xpass_model_json.R

suppressMessages({
  library(xgboost)
  library(jsonlite)
  library(data.table)
  library(arrow)
})
devtools::load_all(".", quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
model_out    <- if (length(args) >= 1) args[1] else file.path(opta_data_dir(), "models", "xpass_model.json")
fixtures_out <- if (length(args) >= 2) args[2] else "data-raw/epv/xpass-fixtures.json"

rds <- "data-raw/cache/epv/xpass_model.rds"
if (!file.exists(rds)) stop("Missing xPass model: ", rds)

obj <- readRDS(rds)
booster <- obj$model
feature_names <- obj$panna_metadata$feature_cols
cat("features (", length(feature_names), "):", paste(feature_names, collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# 1. Raw learner JSON — embed verbatim, NEVER xgb.dump
#
# jsonlite::write_json()/toJSON() re-serialize ANY nested R value handed to
# them; a raw JSON string cannot be passed through untouched (empirically
# confirmed: marking it class="json" just gets it treated as an ordinary
# character scalar and backslash-escaped as a *string*, not spliced as raw
# JSON — this is a known jsonlite footgun, not a hypothetical one). Feeding
# the parsed tree structure back through fromJSON()+toJSON() is also unsafe:
# jsonlite formats whole-number doubles (e.g. a leaf value that happens to be
# exactly 5.0) WITHOUT a decimal point, and xgboost's own strict JSON parser
# distinguishes Integer-typed from Number-typed tokens and rejects the
# former where a float is expected ("Invalid cast, from Integer to Number" —
# reproduced while developing this script). The only lossless option is to
# splice the untouched `rawToChar(xgb.save.raw(...))` bytes directly into a
# hand-built envelope string, never routing them through an R list.
# ---------------------------------------------------------------------------
raw_json_chr <- rawToChar(xgb.save.raw(booster, raw_format = "json"))
parsed <- fromJSON(raw_json_chr, simplifyDataFrame = FALSE, simplifyVector = FALSE)
learner_json <- parsed$learner  # read-only, for metadata extraction below

learner_feature_names <- unlist(learner_json$feature_names)
if (!identical(learner_feature_names, feature_names)) {
  stop("Booster's embedded feature_names disagree with panna_metadata$feature_cols order — ",
       "export would silently mis-order the worker's scoring vector.")
}

obj_name <- learner_json$objective$name
bs_raw <- learner_json$learner_model_param$base_score
base_score <- as.numeric(gsub("[][]", "", bs_raw))
nrounds <- length(learner_json$gradient_booster$model$trees)
cat("objective:", obj_name, "| base_score:", base_score, "| nrounds:", nrounds, "\n")

# Hand-built envelope: every field EXCEPT learner_json goes through jsonlite
# (safe — plain strings/scalars, no tree floats). learner_json is the raw
# xgb.save.raw(raw_format="json") text (both its "learner" and "version" top
# keys — the complete document xgb.load.raw() needs to reconstruct the
# booster) spliced in verbatim, byte for byte.
prefix <- paste0(
  '{',
  '"model_type":"xpass_soccer",',
  '"objective":"', obj_name, '",',
  '"feature_names":', as.character(toJSON(feature_names)), ',',
  '"base_score":', as.character(toJSON(base_score, digits = 17, auto_unbox = TRUE)), ',',
  '"nrounds":', nrounds, ',',
  '"exported_at":"', format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), '",',
  '"learner_json":'
)
envelope_json <- paste0(prefix, raw_json_chr, '}')

dir.create(dirname(model_out), recursive = TRUE, showWarnings = FALSE)
con <- file(model_out, open = "wb")
writeChar(envelope_json, con, eos = NULL, useBytes = TRUE)
close(con)
cat("wrote", model_out, "size:", file.info(model_out)$size, "bytes\n")

# ---------------------------------------------------------------------------
# 2. Fixtures — ~20 REAL passes from cached SPADL (EPV labeled_chunks), not
#    synthetic. labeled_chunks/*.parquet is the same SPADL feed
#    prepare_passes_for_xpass() consumes in the real pipeline.
# ---------------------------------------------------------------------------
chunk_path <- "data-raw/cache/epv/labeled_chunks/chunk_ENG_2025_2026.parquet"
if (!file.exists(chunk_path)) stop("Missing real-pass source chunk: ", chunk_path)

spadl <- as.data.table(read_parquet(chunk_path))
passes <- spadl[action_type == "pass"]
passes[, `:=`(
  pass_distance_raw = sqrt((end_x - start_x)^2 + (end_y - start_y)^2),
  direction = fifelse(end_x - start_x > 0, "forward",
                       fifelse(end_x - start_x < -5, "backward", "lateral")),
  has_head = !is.na(bodypart) & bodypart == "head"
)]
passes[, length_bucket := fifelse(pass_distance_raw < 15, "short",
                                   fifelse(pass_distance_raw < 30, "medium", "long"))]

set.seed(20260720)  # deterministic fixture selection
# Plain logical masks + integer-position indexing (passes[idx]) -- NOT
# quote()/eval() NSE. data.table's `[.data.table` does its own NSE on the i
# argument; wrapping a deferred expression in eval() re-enters that machinery
# and can be misparsed as a join ("i is a data.table (or character vector)")
# depending on call depth/environment. Precomputed logical vectors + which()
# sidestep the ambiguity entirely.
sample_bucket <- function(mask, n) {
  idx <- which(mask)
  if (length(idx) == 0) return(NULL)
  pick_idx <- idx[sample.int(length(idx), min(n, length(idx)))]
  passes[pick_idx]
}
direction <- passes$direction
length_bucket <- passes$length_bucket
result <- passes$result
has_head <- passes$has_head
end_x <- passes$end_x
end_y <- passes$end_y
start_x <- passes$start_x
buckets <- list(
  list(mask = direction == "forward"  & length_bucket == "short"  & result == "success", n = 2),
  list(mask = direction == "forward"  & length_bucket == "short"  & result == "fail",    n = 1),
  list(mask = direction == "forward"  & length_bucket == "medium" & result == "success", n = 2),
  list(mask = direction == "forward"  & length_bucket == "long"   & result == "success", n = 2),
  list(mask = direction == "forward"  & length_bucket == "long"   & result == "fail",    n = 1),
  list(mask = direction == "backward" & length_bucket == "short"  & result == "success", n = 2),
  list(mask = direction == "backward" & length_bucket == "medium" & result == "fail",    n = 1),
  list(mask = direction == "lateral"  & length_bucket == "short"  & result == "success", n = 2),
  list(mask = direction == "lateral"  & length_bucket == "medium" & result == "success", n = 1),
  list(mask = has_head & result == "success", n = 2),
  list(mask = has_head & result == "fail",    n = 1),
  list(mask = end_x > 83 & end_y > 21 & end_y < 79 & result == "success", n = 2),
  list(mask = start_x < 33 & result == "success", n = 1),
  list(mask = start_x >= 67 & result == "fail", n = 2)
)
picked <- rbindlist(lapply(buckets, function(b) sample_bucket(b$mask, b$n)), fill = TRUE)
picked <- unique(picked, by = c("match_id", "action_id"))
picked <- head(picked, 22)
cat("fixture passes selected:", nrow(picked),
    "| completed:", sum(picked$result == "success"),
    "| failed:", sum(picked$result == "fail"), "\n")

featured <- create_pass_features(copy(picked))
X <- as.matrix(featured[, ..feature_names])
storage.mode(X) <- "double"
preds <- predict(booster, X)

fixtures <- vector("list", nrow(featured))
for (i in seq_len(nrow(featured))) {
  input_features <- as.list(featured[i, ..feature_names])
  fixtures[[i]] <- list(
    match_id = featured$match_id[i],
    action_id = featured$action_id[i],
    player_id = featured$player_id[i],
    player_name = featured$player_name[i],
    actual_result = featured$result[i],
    input_features = input_features,
    expected_completion_probability = preds[i]
  )
}
fixtures_envelope <- list(
  model_type = "xpass_soccer",
  source = chunk_path,
  feature_names = feature_names,
  n_fixtures = length(fixtures),
  fixtures = fixtures
)
write_json(fixtures_envelope, fixtures_out, auto_unbox = TRUE, digits = 17, pretty = FALSE)
cat("wrote", fixtures_out, "size:", file.info(fixtures_out)$size, "bytes\n")

# ---------------------------------------------------------------------------
# 3. Round-trip verification — reload the FILE we just wrote (not the
#    in-memory raw_json_chr) by extracting the exact byte range of the
#    "learner_json" value (we authored `prefix` ourselves, so its length is
#    known precisely — no brace-matching/parsing needed), confirm those bytes
#    are byte-identical to xgb.save.raw()'s original output, feed them to
#    xgb.load.raw(), and confirm fixture predictions match the original
#    booster EXACTLY (max abs diff must be 0, not just "close").
# ---------------------------------------------------------------------------
file_full_text <- readChar(model_out, file.info(model_out)$size, useBytes = TRUE)
Encoding(file_full_text) <- "UTF-8"
if (!startsWith(file_full_text, prefix)) {
  stop("Written file does not start with the expected envelope prefix — export is corrupt.")
}
extracted_learner_json <- substr(file_full_text, nchar(prefix) + 1, nchar(prefix) + nchar(raw_json_chr))
if (!identical(extracted_learner_json, raw_json_chr)) {
  stop("Bytes read back from ", model_out, " for learner_json are NOT byte-identical to ",
       "xgb.save.raw()'s original output — export corrupted the model JSON, do not ship.")
}
cat("verified: learner_json in the written file is byte-identical to xgb.save.raw() output\n")

reloaded_booster <- xgb.load.raw(charToRaw(extracted_learner_json))
preds_reloaded <- predict(reloaded_booster, X)
max_abs_diff <- max(abs(preds - preds_reloaded))
cat("round-trip max abs diff (", length(preds), "fixtures):", format(max_abs_diff, scientific = TRUE), "\n")
if (max_abs_diff > 0) {
  stop("Round-trip predictions do not match the original booster exactly (max abs diff = ",
       max_abs_diff, ") — export is not lossless, do not ship.")
}
cat("ROUND-TRIP OK: exported JSON reproduces the original booster's predictions exactly.\n")
