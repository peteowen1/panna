# Orientation-symmetric match rows
# =================================
# The match-prediction models use home_*/away_* feature columns. Trained naively
# on one row per match, an XGBoost model learns to associate the home_* column
# position itself with winning (62k+ non-neutral games where the home side has a
# real edge), leaving a residual "home tilt" even for genuinely neutral games.
#
# The fix is data augmentation by symmetry: train on every match PLUS its
# home/away-swapped mirror. With both orientations equally represented the model
# cannot bias toward a column position. Home advantage is preserved because it
# is carried by the signed `home_field` feature (+1 host / 0 neutral / -1
# visitor), which the mirror negates.


#' Mirror match rows (swap home/away perspective)
#'
#' Produces the home/away-swapped version of a set of match rows. Every
#' \code{home_*} column is exchanged with its \code{away_*} partner; signed
#' "home-perspective" columns (\code{*_diff}, \code{diff_*}, \code{home_field},
#' \code{pred_goal_diff}) are negated; and the result/outcome labels are
#' flipped. Symmetric quantities (\code{pred_total_goals}, league dummies,
#' \code{is_neutral_venue}, \code{match_month}, ...) are left unchanged.
#'
#' Used in two places:
#' \itemize{
#'   \item Steps 05/06 — append \code{rbind(train, mirror_match_rows(train))}
#'         so the goals and outcome models train on both orientations.
#'   \item Step 07 — predict each fixture in both orientations and average,
#'         giving a prediction invariant to which team is listed as home.
#' }
#'
#' @param df A data.frame (or data.table) of match rows. Any subset of the
#'   standard match-dataset columns is accepted; only the columns that are
#'   present are transformed.
#' @return A data.frame of identical shape and column order with the
#'   home/away perspective swapped.
#' @export
mirror_match_rows <- function(df) {
  df  <- as.data.frame(df)
  out <- df
  nm  <- names(df)

  # 1. Swap every paired home_X <-> away_X column. This covers features
  #    (home_sum_panna, ...), goals/xg (home_goals, home_xg), identity
  #    (home_team, home_team_id) and rolling form (home_*_last_N). Unpaired
  #    home_* columns (e.g. home_field) are skipped here and handled below.
  for (hc in grep("^home_", nm, value = TRUE)) {
    ac <- sub("^home_", "away_", hc)
    if (ac %in% nm) {
      out[[hc]] <- df[[ac]]
      out[[ac]] <- df[[hc]]
    }
  }
  # pred_home_goals <-> pred_away_goals (present at the outcome-model stage)
  if (all(c("pred_home_goals", "pred_away_goals") %in% nm)) {
    out[["pred_home_goals"]] <- df[["pred_away_goals"]]
    out[["pred_away_goals"]] <- df[["pred_home_goals"]]
  }

  # 2. Negate signed home-perspective columns: every *_diff / diff_* column
  #    is (home - away) so it flips sign, and home_field flips host<->visitor.
  neg <- grep("(_diff$|^diff_)", nm, value = TRUE)
  if ("home_field" %in% nm) neg <- union(neg, "home_field")
  for (dc in neg) {
    if (is.numeric(df[[dc]])) out[[dc]] <- -df[[dc]]
  }

  # 3. Flip the outcome labels (home win <-> away win; draw unchanged).
  if ("result" %in% nm) {
    out[["result"]] <- unname(c(H = "A", D = "D", A = "H")[df[["result"]]])
  }
  if ("outcome_label" %in% nm) {
    # 0 = home win, 1 = draw, 2 = away win
    out[["outcome_label"]] <- unname(
      c(`0` = 2L, `1` = 1L, `2` = 0L)[as.character(df[["outcome_label"]])])
  }

  out
}
