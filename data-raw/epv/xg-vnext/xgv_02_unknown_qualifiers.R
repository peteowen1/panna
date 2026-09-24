# xG next round, step 2: what are Opta's undocumented shot qualifiers?
# Public F24 lists stop around id 217. For each newer id on shots: the values it
# carries, the seasons it appears in, its goal rate, and which documented tags it
# co-occurs with -- enough to say whether it is known BEFORE the shot.
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_02_unknown_qualifiers.R
suppressMessages({library(data.table); library(arrow); library(dplyr); library(jsonlite)})
OD <- "C:/dev/pannaverse/pannadata/data/opta/"
IDS <- c("193","230","231","233","254","266","273","274","275","276","280","281","282","284","286",
         "300","307","314","328","343","362","374","375","388","389","391","395","396","458","468","472","492")
KNOWN <- c(`214` = "big chance", `215` = "individual play", `23` = "fast break", `89` = "1 on 1",
           `29` = "assisted", `154` = "intentional assist", `15` = "head", `82` = "blocked",
           `133` = "deflection", `22` = "regular play", `25` = "corner", `24` = "set piece")
ev <- as.data.table(collect(open_dataset(list.files(paste0(OD, "events_consolidated"), "^events_.*[.]parquet$", full.names = TRUE)) |>
  filter(type_id %in% c(13, 14, 15, 16), competition %in% c("EPL", "La_Liga", "Bundesliga", "Serie_A", "Ligue_1", "MLS", "Championship")) |>
  select(match_id, event_id, type_id, x, y, period_id, season, qualifier_json)))
ev <- ev[period_id %in% 1:4 & !is.na(qualifier_json)]
ev[, goal := as.integer(type_id == 16)]
ev[, yr := suppressWarnings(as.integer(substr(season, nchar(season) - 3, nchar(season))))]
q <- lapply(ev$qualifier_json, function(s) tryCatch(fromJSON(s), error = function(e) list()))
has <- function(id) vapply(q, function(z) id %in% names(z), logical(1))
val <- function(id) vapply(q, function(z) { v <- z[[id]]; if (is.null(v)) NA_character_ else as.character(v) }, character(1))
cat("shots:", nrow(ev), "(big 7 competitions, periods 1-4)\n")
known_h <- vapply(names(KNOWN), has, logical(nrow(ev)))
for (id in IDS) {
  h <- has(id); if (sum(h) < 200) next
  v <- val(id)[h]
  first_yr <- ev[h, min(yr, na.rm = TRUE)]
  by_type <- ev[h, .N, by = type_id][order(type_id)]
  co <- colMeans(known_h[h, , drop = FALSE]) - colMeans(known_h[!h, , drop = FALSE])
  top_co <- head(co[order(-abs(co))], 3)
  cat(sprintf("\n== %s: %d shots (%.1f%%), goal rate %.3f (others %.3f), first seen %s\n", id, sum(h), 100 * mean(h),
              mean(ev$goal[h]), mean(ev$goal[!h]), first_yr))
  cat("   by outcome type (13 miss,14 post,15 saved/blocked,16 goal):", paste(by_type$type_id, by_type$N, sep = ":", collapse = " "), "\n")
  nv <- suppressWarnings(as.numeric(v))
  if (mean(!is.na(nv)) > 0.9) cat("   numeric values: min", min(nv, na.rm = TRUE), " median", median(nv, na.rm = TRUE), " max", max(nv, na.rm = TRUE),
                                  " | cor with shot x", round(cor(nv, ev$x[h], use = "complete.obs"), 2), " y", round(cor(nv, ev$y[h], use = "complete.obs"), 2), "\n")
  else cat("   values:", paste(head(names(sort(table(v, useNA = "ifany"), decreasing = TRUE)), 6), collapse = " | "), "\n")
  cat("   travels with (share with minus share without):", paste(sprintf("%s %+.2f", KNOWN[names(top_co)], top_co), collapse = ", "), "\n")
}
