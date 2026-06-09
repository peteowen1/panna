# scrape_wiki_squads.R
# Scrape WC2026 squads directly from the canonical Wikipedia page and
# rebuild data-raw/cache-predictions-opta/wc2026_announced_squads.parquet.
#
# Why this exists: the hard-coded WC2026_ANNOUNCED_SQUADS list in
# announced_squads.R drifts the moment a federation announces or amends a
# squad. Wikipedia is updated within hours by editors who cross-check the
# federation press release, so it's the lowest-effort canonical source.
#
# Usage (refresh + rebuild parquet, idempotent):
#   cd panna && Rscript data-raw/match-predictions-opta/scrape_wiki_squads.R
#
# What it does:
#   1. Downloads the raw wikitext for "2026 FIFA World Cup squads"
#      (action=raw — no rendering, no auth, ~225 KB).
#   2. Walks the wikitext, splitting on `===TeamName===` headers and
#      reading each `{{nat fs g player|name=[[NAME]]|…}}` line.
#   3. Tags a team as FINAL when it has 23–26 player rows (FIFA's allowed
#      range); else PRELIMINARY (will be picked up next refresh).
#   4. Maps Wikipedia team names → Opta `team_name` (the resolver in
#      announced_squads.R needs the Opta spelling).
#   5. Hands the named list off to `build_wc2026_announced_squads()`,
#      which resolves names → player_ids via opta_lineups and writes the
#      parquet (announced + derived squads in one file).
#
# Run any time. To re-run tomorrow / next week:
#   Rscript data-raw/match-predictions-opta/scrape_wiki_squads.R

suppressPackageStartupMessages({
  library(data.table)
  devtools::load_all(".")
})

wiki_url <- "https://en.wikipedia.org/w/index.php?title=2026_FIFA_World_Cup_squads&action=raw"
cache_path <- file.path("data-raw", "cache-predictions-opta",
                         "wc2026_wiki_squads_raw.txt")
dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)

# 1. Download wikitext ---------------------------------------------------

message(sprintf("Downloading Wikipedia squads page -> %s", cache_path))
ua <- "Mozilla/5.0 (compatible; panna-pipeline/1.0)"
# Use httr2 with explicit UA — Wikipedia 403s anonymous user-agents.
req <- httr2::request(wiki_url)
req <- httr2::req_user_agent(req, ua)
req <- httr2::req_retry(req, max_tries = 3L, backoff = function(n) 2^n)
resp <- httr2::req_perform(req)
httr2::resp_check_status(resp)
wikitext <- httr2::resp_body_string(resp)
writeLines(wikitext, cache_path)
# Round-trip via the file: writeLines + readLines normalises Wikipedia's
# \r\n endings, which is what the anchored regexes (^===…===$) rely on.
lines <- readLines(cache_path, warn = FALSE)
message(sprintf("  wrote %.0f KB (%d lines)", nchar(wikitext) / 1024,
                length(lines)))

# 2. Parse wikitext into named list ------------------------------------
parsed <- list()
current <- NULL
for (ln in lines) {
  # === Team Name === starts a new section
  if (grepl("^===[^=]+===$", ln)) {
    current <- gsub("^===|===$", "", ln)
    parsed[[current]] <- character(0)
  } else if (grepl("^==[^=]+==$", ln)) {
    # ==Group X== or ==Notes== — leave current as-is (might end a team
    # section), but don't reset to NULL until we see a new team header.
    current <- NULL
  } else if (!is.null(current) && grepl("\\{\\{nat fs g player", ln)) {
    # Two name-extraction passes:
    #   (1) |name=[[Wiki Page|Display]]  -> players with a Wikipedia article
    #   (2) |name=Plain Name             -> players without one (common for
    #       low-EM call-ups from Cabo Verde / Haiti / Curaçao etc.)
    # Pass 1 was the only one before; missing pass 2 silently dropped 3-8
    # players per smaller-nation squad, which inflated the EM weight on the
    # few resolved names and biased team strength low. Code-review item 15.
    # Use perl + lazy quantifiers; R's POSIX engine handles the
    # `[^\\]]*` character class inconsistently and silently misses lines.
    raw <- NA_character_
    m1 <- regmatches(ln, regexpr("\\|name=\\[\\[(.*?)\\]\\]", ln, perl = TRUE))
    if (length(m1) == 1L) {
      raw <- sub("^\\|name=\\[\\[", "", m1)
      raw <- sub("\\]\\]$", "", raw)
      if (grepl("|", raw, fixed = TRUE)) {
        parts <- strsplit(raw, "|", fixed = TRUE)[[1]]
        raw <- parts[length(parts)]
      }
    } else {
      # Pass 2: no double-brackets. Stop the name at the next `|` or `}}`.
      m2 <- regmatches(ln, regexpr("\\|name=([^|}]+)", ln, perl = TRUE))
      if (length(m2) == 1L) {
        raw <- trimws(sub("^\\|name=", "", m2))
      }
    }
    if (!is.na(raw) && nzchar(raw)) {
      parsed[[current]] <- c(parsed[[current]], raw)
    }
  }
}

# Drop non-team sections (==Coaches==, sortname helpers, etc.).
parsed <- parsed[lengths(parsed) > 0L]

# 3. Split into FINAL (23-26 players) and PRELIMINARY ------------------

squad_sizes <- vapply(parsed, length, integer(1))
is_final <- squad_sizes >= 23L & squad_sizes <= 26L
final_teams <- names(parsed)[is_final]
prelim_teams <- names(parsed)[!is_final]

message(sprintf("\nWikipedia parse: %d total team-sections", length(parsed)))
message(sprintf("  FINAL (23-26 players): %d teams", length(final_teams)))
for (t in final_teams) message(sprintf("    [%s] %d", t, length(parsed[[t]])))
message(sprintf("  PRELIMINARY (>26 or <23): %d teams (will be derived from opta history)",
                length(prelim_teams)))
for (t in prelim_teams) message(sprintf("    [%s] %d", t, length(parsed[[t]])))

# 4. Wikipedia -> Opta team-name mapping --------------------------------
# Wikipedia uses common English names; opta_lineups uses different
# variants for a handful of teams. Keys = Wikipedia name, values = Opta
# team_name. Any team not in this map is assumed to share its Wikipedia
# spelling with Opta.
wiki_to_opta <- c(
  "Bosnia and Herzegovina" = "Bosnia-Herzegovina",
  "Czech Republic"         = "Czechia",
  "South Korea"            = "Korea Republic",
  "Ivory Coast"            = "Côte d'Ivoire",
  "Cape Verde"             = "Cabo Verde",
  "DR Congo"               = "Congo DR",
  "Iran"                   = "IR Iran",
  "Turkey"                 = "Türkiye"
)

remap <- function(nm) {
  if (nm %in% names(wiki_to_opta)) wiki_to_opta[[nm]] else nm
}
opta_named <- setNames(parsed[final_teams],
                       vapply(final_teams, remap, character(1)))

# 5. Hand off to the existing builder ----------------------------------

source("data-raw/match-predictions-opta/announced_squads.R", local = TRUE)
out <- build_wc2026_announced_squads(squads = opta_named)

message(sprintf("\nDone. %d rows in wc2026_announced_squads.parquet.",
                nrow(out)))
