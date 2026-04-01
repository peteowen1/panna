# 00_add_gk_per90_cols.R
# One-time script to add missing GK per90 columns to the cached match stats parquet.
# Run BEFORE 07_train_psr_model.R on a machine with enough RAM (>16GB recommended).
#
# The new columns are: saves_ibox_p90, saves_obox_p90, high_claim_p90,
# good_high_claim_p90, punches_p90, keeper_throws_p90, keeper_throws_accuracy
#
# These are needed by the GK sub-model (.get_gk_skill_cols() in psr.R).

library(arrow)

pq_path <- "data-raw/cache-skills/opta_match_stats.parquet"
cat("Reading", pq_path, "...\n")
df <- read_parquet(pq_path)
cat(sprintf("  %s rows x %s cols\n", format(nrow(df), big.mark = ","), ncol(df)))

# Check which GK per90 cols are already present
new_p90_cols <- c("saves_ibox_p90", "saves_obox_p90", "high_claim_p90",
                  "good_high_claim_p90", "punches_p90", "keeper_throws_p90")
existing <- new_p90_cols[new_p90_cols %in% names(df)]
missing  <- new_p90_cols[!new_p90_cols %in% names(df)]

if (length(missing) == 0 && "keeper_throws_accuracy" %in% names(df)) {
  cat("All GK per90 columns already present. Nothing to do.\n")
} else {
  m90 <- df$total_minutes / 90
  m90[m90 == 0 | is.na(m90)] <- NA_real_

  # Compute missing per90 columns from raw counts
  raw_to_p90 <- c(
    "saves_ibox_p90"       = "saves_ibox",
    "saves_obox_p90"       = "saves_obox",
    "high_claim_p90"       = "high_claim",
    "good_high_claim_p90"  = "good_high_claim",
    "punches_p90"          = "punches",
    "keeper_throws_p90"    = "keeper_throws"
  )

  for (p90_name in names(raw_to_p90)) {
    raw_name <- raw_to_p90[[p90_name]]
    if (p90_name %in% missing && raw_name %in% names(df)) {
      df[[p90_name]] <- df[[raw_name]] / m90
      cat(sprintf("  Added %s\n", p90_name))
    }
  }

  # keeper_throws_accuracy = keeper_throws_acc / keeper_throws
  if (!"keeper_throws_accuracy" %in% names(df)) {
    kt <- df$keeper_throws
    kt[kt == 0 | is.na(kt)] <- NA_real_
    df$keeper_throws_accuracy <- df$keeper_throws_acc / kt
    cat("  Added keeper_throws_accuracy\n")
  }

  cat(sprintf("Writing %s rows x %s cols...\n",
              format(nrow(df), big.mark = ","), ncol(df)))
  write_parquet(df, pq_path)
  cat("Done.\n")
}
