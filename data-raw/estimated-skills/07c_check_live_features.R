# Cross-repo lockstep guard for 07c's LIVE_OBSERVABLE_FEATURES (panna#123).
#
# The list is a hand transcription of what the blog's live scorer can actually
# build: stat-value.js's PER90 map restricted to Opta columns its
# eventFeaturesFromRows derives, plus the two thin-block-supplied extras
# (ontargetScoringAtt / goalsConceded are deliberately NOT event-derived — the
# Opta block ships them even for thin feeds). Hand copies drift: the panna#123
# review caught blocked_passes_p90 wrongly in / corners+setpiece+fastbreak
# wrongly out. This script recomputes the set from the blog source and fails
# loudly on any coefficient-relevant difference.
#
# Run weekly by psr-weekly-snapshot.yml with STAT_VALUE_JS pointing at a fresh
# fetch of inthegame-blog/football/stat-value.js. Local check:
#   STAT_VALUE_JS=C:/dev/inthegame-blog/football/stat-value.js \
#     Rscript data-raw/estimated-skills/07c_check_live_features.R
suppressMessages({ library(data.table) })
devtools::load_all(".", quiet = TRUE)

js_path <- Sys.getenv("STAT_VALUE_JS", "")
if (!nzchar(js_path) || !file.exists(js_path))
  stop("STAT_VALUE_JS must point at the blog's football/stat-value.js")
js <- readLines(js_path, warn = FALSE)

# --- PER90 map: panna feature -> Opta matchstats column -----------------------
map_start <- grep("var PER90 = \\{", js)
if (length(map_start) != 1L) stop("could not locate the PER90 map in stat-value.js")
map_end <- map_start + which(grepl("^\\s*\\}", js[(map_start + 1):length(js)]))[1]
map_txt <- paste(js[map_start:map_end], collapse = " ")
pairs <- regmatches(map_txt, gregexpr('([a-z0-9_]+):\\s*"([A-Za-z0-9]+)"', map_txt))[[1]]
per90_map <- setNames(sub('.*"([A-Za-z0-9]+)"', "\\1", pairs),
                      sub(':.*', "", pairs))
if (length(per90_map) < 40L)
  stop(sprintf("parsed only %d PER90 entries — stat-value.js layout changed?", length(per90_map)))

# --- Opta columns eventFeaturesFromRows derives -------------------------------
fn_start <- grep("function eventFeaturesFromRows", js)
fn_end <- grep("function eventToOpta", js)
if (length(fn_start) != 1L || length(fn_end) != 1L || fn_end <= fn_start)
  stop("could not bound eventFeaturesFromRows in stat-value.js")
body <- js[fn_start:(fn_end - 1L)]
add_lines <- body[grepl("add\\(pid", body)]
derived <- unique(unlist(regmatches(add_lines, gregexpr('"([A-Za-z0-9]+)"', add_lines))))
derived <- gsub('"', "", derived)
if (length(derived) < 20L)
  stop(sprintf("parsed only %d derived Opta cols — stat-value.js layout changed?", length(derived)))

# Thin-block-supplied features the derivation deliberately leaves to the block
BLOCK_SUPPLIED <- c("shots_on_target_p90", "goals_conceded_p90")
expected <- union(names(per90_map)[per90_map %in% derived], BLOCK_SUPPLIED)

# --- 07c's transcribed list ----------------------------------------------------
src <- readLines("data-raw/estimated-skills/07c_build_live_psv_constants.R", warn = FALSE)
# eval() here is safe: it evaluates c("...") literals from OUR OWN checked-in
# 07c script (same repo, same commit). The externally fetched stat-value.js is
# only ever regex-parsed above, never evaluated.
# LIVE_OBSERVABLE_FEATURES references LIVE_XMETRICS_FEATURES (7473e33), so
# both assignments are extracted and evaluated in one environment — the old
# single-assignment eval died with "object 'LIVE_XMETRICS_FEATURES' not found".
lst_env <- new.env(parent = baseenv())
.eval_07c_assign <- function(name) {
  a_start <- grep(sprintf("^%s <- c\\($", name), src)
  if (length(a_start) != 1L) stop(sprintf("could not locate %s in 07c", name))
  a_end <- a_start + which(grepl("^\\)$", src[(a_start + 1):length(src)]))[1]
  eval(parse(text = paste(src[a_start:a_end], collapse = "\n")), envir = lst_env)
}
.eval_07c_assign("LIVE_XMETRICS_FEATURES")
.eval_07c_assign("LIVE_OBSERVABLE_FEATURES")
mine <- lst_env$LIVE_OBSERVABLE_FEATURES

# --- Compare, restricted to features that carry coefficients -------------------
all_coef_stats <- unique(unlist(lapply(
  list(c("margin","blend","outfield"), c("offense","blend","outfield"),
       c("defense","blend","outfield"), c("margin","goals","gk"),
       c("offense","goals","gk"), c("defense","goals","gk")),
  function(a) load_psr_coefficients(a[1], target = a[2], model = a[3])$stat_name)))

missing_from_07c <- sort(intersect(setdiff(expected, mine), all_coef_stats))
not_derivable   <- sort(intersect(setdiff(mine, expected), all_coef_stats))

if (length(missing_from_07c) + length(not_derivable) > 0) {
  stop(sprintf(paste(
    "LIVE_OBSERVABLE_FEATURES drifted from the blog's stat-value.js.",
    "Blog-derivable but MISSING from 07c (constants over-absorb them): %s.",
    "In 07c but NOT blog-derivable (constants under-absorb them): %s.",
    "Update the list in 07c AND regenerate psv_live_constants.csv."),
    if (length(missing_from_07c)) paste(missing_from_07c, collapse = ", ") else "none",
    if (length(not_derivable)) paste(not_derivable, collapse = ", ") else "none"))
}
cat(sprintf("lockstep OK: %d observable features match stat-value.js (%d derived cols, %d PER90 entries)\n",
            length(intersect(mine, all_coef_stats)), length(derived), length(per90_map)))
