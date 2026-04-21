# EPV Component Sanity Check
# ===========================
# Spot-checks each EPV component against known-archetype players to confirm
# the decomposition is picking up what we expect. Reads from the already-built
# game_logs_*.parquet files under data-raw/cache-predictions-opta/.
#
# Run: cd panna && Rscript data-raw/analysis/epv_component_sanity.R
# Output: prints top-20 tables to stdout + writes an .md report under
#         data-raw/analysis/epv_component_sanity_report.md
#
# What we check:
#   epv_passing   → playmakers: De Bruyne, Ødegaard, Kimmich, Modrić
#   epv_shooting  → finishers : Haaland, Kane, Mbappé, Lewandowski
#   epv_dribbling → carriers  : Messi, Vinicius, Saka, Leão
#   epv_defending → defenders : Rodri, Van Dijk, Casemiro, Fabinho
#   epv_as_receiver → players who get found in dangerous spots
#   epv_offensive/defensive → broad buckets

suppressMessages({
  library(data.table)
  library(arrow)
})

cache_dir <- "data-raw/cache-predictions-opta"
out_md    <- "data-raw/analysis/epv_component_sanity_report.md"

files <- list.files(cache_dir, pattern = "^game_logs_\\d{4}-\\d{4}\\.parquet$",
                    full.names = TRUE)
if (length(files) == 0) {
  stop("No per-season game_logs parquets found in ", cache_dir)
}

message(sprintf("Loading %d season files...", length(files)))
all_logs <- rbindlist(lapply(files, function(f) {
  as.data.table(read_parquet(f))
}), fill = TRUE)

message(sprintf("  %d player-games across %d seasons, %d leagues",
                nrow(all_logs), length(unique(all_logs$season)),
                length(unique(all_logs$league))))

# ---- Aggregate across all seasons: min-weighted totals + per-90 ----

component_cols <- c("epv_total", "epv_passing", "epv_shooting", "epv_dribbling",
                    "epv_defending")
# offense / defense are the renamed epv_offensive / epv_defensive
offense_col <- if ("offense" %in% names(all_logs)) "offense" else "epv_offensive"
defense_col <- if ("defense" %in% names(all_logs)) "defense" else "epv_defensive"
keep_cols <- intersect(c(component_cols, offense_col, defense_col), names(all_logs))

agg <- all_logs[, c(
  list(
    n_games = .N,
    total_minutes = sum(total_minutes, na.rm = TRUE),
    seasons = paste(sort(unique(season)), collapse = ",")
  ),
  lapply(.SD, sum, na.rm = TRUE)
), by = .(player_id, player_name), .SDcols = keep_cols]

# Per-90 versions
mins_safe <- pmax(agg$total_minutes, 1)
for (col in keep_cols) {
  set(agg, j = paste0(col, "_p90"), value = agg[[col]] / (mins_safe / 90))
}

# Filter to players with meaningful sample (>= 30 full games = 2700 min)
qualified <- agg[total_minutes >= 2700]
message(sprintf("  %d players qualified (>=2700 min across history)",
                nrow(qualified)))

# ---- Helper: print & capture top-N by a metric ----

leaderboard <- function(dt, col, n = 20, by = c("total", "p90"),
                        extra_cols = c("n_games", "total_minutes")) {
  by <- match.arg(by)
  metric_col <- if (by == "p90") paste0(col, "_p90") else col
  out <- dt[order(-get(metric_col))][1:min(n, nrow(dt))]
  display_cols <- c("player_name", metric_col, extra_cols)
  out <- out[, ..display_cols]
  out[]
}

# ---- Print leaderboards ----

md_lines <- c(
  "# EPV Component Sanity Report",
  "",
  sprintf("Generated: %s", Sys.time()),
  sprintf("Data: %d player-games across %d seasons, %d leagues",
          nrow(all_logs), length(unique(all_logs$season)),
          length(unique(all_logs$league))),
  sprintf("Qualification: >=2700 min (~30 full games) — %d players",
          nrow(qualified)),
  "",
  "---",
  ""
)

print_section <- function(title, col, explain) {
  cat("\n========== ", title, " ==========\n", sep = "")
  cat(explain, "\n\n")

  cat("-- Top 20 by career total --\n")
  top_tot <- leaderboard(qualified, col, 20, by = "total")
  print(top_tot, row.names = FALSE)

  cat("\n-- Top 20 by per-90 (>=2700 min) --\n")
  top_p90 <- leaderboard(qualified, col, 20, by = "p90")
  print(top_p90, row.names = FALSE)

  md_lines <<- c(
    md_lines,
    sprintf("## %s (%s)", title, col),
    "",
    explain,
    "",
    "### Top 20 — Career total",
    "",
    paste(capture.output(print(top_tot, row.names = FALSE)), collapse = "\n"),
    "",
    "### Top 20 — Per-90 (qualified)",
    "",
    paste(capture.output(print(top_p90, row.names = FALSE)), collapse = "\n"),
    "",
    "---",
    ""
  )
}

print_section("PASSING", "epv_passing",
  "Expect playmakers: Kevin De Bruyne, Martin Ødegaard, Joshua Kimmich, Luka Modrić, Bruno Fernandes, Bernardo Silva, Toni Kroos.")

print_section("SHOOTING", "epv_shooting",
  "Expect finishers: Erling Haaland, Harry Kane, Robert Lewandowski, Kylian Mbappé, Mohamed Salah, Lionel Messi, Cristiano Ronaldo.")

print_section("DRIBBLING", "epv_dribbling",
  "Expect dribble-carriers: Messi, Vinicius Jr, Bukayo Saka, Rafael Leão, Ousmane Dembélé, Jeremy Doku.")

print_section("DEFENDING", "epv_defending",
  "Expect defenders + holding mids: Rodri, Virgil van Dijk, Casemiro, Fabinho, N'Golo Kanté, Rúben Dias, Declan Rice.")

print_section("OFFENSE (overall)", offense_col,
  "Expect top attackers (shooters + playmakers + receivers): Haaland, Mbappé, Messi, Ronaldo, De Bruyne, Lewandowski.")

print_section("DEFENSE (overall)", defense_col,
  "Per-match convention: POSITIVE = good defender (opposite of season-level blog 'defense' which is sign-flipped). Expect top centre-backs and holding mids.")

# ---- Bonus: cross-component sanity. A striker should score HIGH on shooting
# ---- but LOW on defending. A CDM should be the inverse. ----

cat("\n========== CROSS-COMPONENT PROFILES ==========\n")
cat("Top 10 'imbalance' — highest shooting minus defending per-90:\n\n")

qualified[, imbalance_attacking := epv_shooting_p90 - epv_defending_p90]
qualified[, imbalance_defending := epv_defending_p90 - epv_shooting_p90]

top_attack <- qualified[order(-imbalance_attacking)][1:10, .(
  player_name, epv_shooting_p90, epv_defending_p90, imbalance_attacking,
  total_minutes
)]
print(top_attack, row.names = FALSE)

cat("\nTop 10 by defending minus shooting (pure defenders):\n\n")
top_def <- qualified[order(-imbalance_defending)][1:10, .(
  player_name, epv_defending_p90, epv_shooting_p90, imbalance_defending,
  total_minutes
)]
print(top_def, row.names = FALSE)

md_lines <- c(
  md_lines,
  "## Cross-component profiles",
  "",
  "Strikers should score high on `epv_shooting_p90` and low on `epv_defending_p90` — and vice versa for defenders. This is a sanity check that the component split picks up archetypes rather than just volume of actions.",
  "",
  "### Most attack-skewed (shooting minus defending, per-90)",
  "",
  paste(capture.output(print(top_attack, row.names = FALSE)), collapse = "\n"),
  "",
  "### Most defense-skewed (defending minus shooting, per-90)",
  "",
  paste(capture.output(print(top_def, row.names = FALSE)), collapse = "\n"),
  ""
)

# ---- Write markdown report ----
writeLines(md_lines, out_md)
cat(sprintf("\n\nWrote report: %s\n", out_md))
