# Full-model knockout match probabilities
# ========================================
# The group stage predicts each known fixture with the full 170-feature
# XGBoost goals + outcome models. Knockout matchups are not known until the
# bracket fills in mid-simulation, so the simulator historically compressed
# every team into a single Bradley-Terry rating.
#
# But every feature the model uses is a TEAM-LEVEL property (squad panna/PSR/
# EPR aggregates, team Elo, rolling form) -- nothing is matchup-specific except
# the home/away diffs and the venue flags. So any matchup A-vs-B can be
# assembled from the two teams' feature vectors. There are only 48*47/2 = 1128
# possible knockout matchups: we predict them ALL once with the full model and
# store a lookup table. The simulator then gets full-fidelity knockout
# probabilities at hash-lookup speed -- no BT compression.


#' @keywords internal
.ko_predict <- function(X, goals_models, outcome_result, augmented_features) {
  # KEEP THIS. It is not a lazy fallback -- it is train/serve parity. The
  # goals and outcome models are trained on zero-imputed matrices
  # (data-raw/match-predictions-opta/05_fit_goals_model.R:68 and
  # 06_fit_outcome_model.R:55,95 both do `X[is.na(X)] <- 0` before fitting),
  # so XGBoost never sees a missing value in training and has no learned
  # default direction to fall back on. Every serving path imputes the same
  # way: 07_predict_fixtures.R:95,99,131 and 08_evaluate_model.R:42.
  # Removing it here would make the knockout path the ONLY one feeding raw
  # NAs to models that were never trained on them. It matters: 112 of 177
  # feature columns carry NAs in the WC rows this function predicts, at up to
  # 35% (away_xg). If you want native NA handling, drop the imputation in 05
  # and 06 and retrain -- changing it on the serving side alone is a bug.
  X[is.na(X)] <- 0
  d <- xgboost::xgb.DMatrix(data = X)
  hg <- stats::predict(goals_models$home$model, d)
  ag <- stats::predict(goals_models$away$model, d)
  gf <- cbind(pred_home_goals = hg, pred_away_goals = ag,
              pred_goal_diff = hg - ag, pred_total_goals = hg + ag)
  Xo <- cbind(X, gf)
  # `augmented_features` is defined as feature_cols + the four goal columns
  # (06_fit_outcome_model.R:39), and Xo carries exactly those, so this set is
  # structurally empty. A non-empty one means the goals and outcome models
  # were fitted from different vintages of feature_cols. Zero-filling the gap
  # used to hide that: predictions stayed plausible while N features silently
  # read as 0. Fail instead -- the fix is to refit, not to pad.
  miss <- setdiff(augmented_features, colnames(Xo))
  if (length(miss)) {
    cli::cli_abort(c(
      "Goals and outcome models disagree on the feature set.",
      "x" = "{length(miss)} of {length(augmented_features)} outcome-model feature{?s} absent from the goals-model matrix.",
      "i" = "Missing: {.field {utils::head(miss, 10)}}{if (length(miss) > 10) ' ...' else ''}",
      "i" = "Refit steps 05 and 06 from the same match dataset."
    ))
  }
  Xo <- Xo[, augmented_features, drop = FALSE]
  pr <- softprob_matrix(
    stats::predict(outcome_result$model$model, xgboost::xgb.DMatrix(data = Xo)),
    nrow(Xo)
  )
  list(hg = hg, ag = ag, pH = pr[, 1], pD = pr[, 2], pA = pr[, 3])
}


#' Build the full-model knockout matchup lookup
#'
#' Predicts every possible pairwise knockout matchup with the full goals +
#' outcome models, so the World Cup simulator can use full-fidelity
#' probabilities for knockout ties instead of a compressed Bradley-Terry
#' rating.
#'
#' Each matchup row is assembled from the two teams' feature vectors (extracted
#' from the WC2026 rows of the match dataset), with the home/away diff columns
#' recomputed and a host-aware \code{home_field}. Predictions are symmetrized
#' (both orientations averaged) so they do not depend on listing order.
#'
#' @param match_dataset The step-04 match dataset (has every team's WC2026
#'   feature rows).
#' @param goals_models Step-05 goals models -- with \code{$feature_cols}
#'   top-level and \code{$pooled} / \code{$international} sub-objects each
#'   holding \code{$home} / \code{$away}.
#' @param outcome_result Step-06 outcome models -- with
#'   \code{$augmented_features} top-level and \code{$pooled} /
#'   \code{$international} sub-objects each holding \code{$model}.
#' @param season WC season string used to locate the team rows.
#' @param hosts Host nations that get \code{home_field} advantage.
#' @param verbose Print progress.
#' @details World Cup matchups are international, so each matchup is predicted
#'   as a blend of the pooled and international-specialist models
#'   (\code{MATCH_INTL_BLEND_WEIGHT} on the specialist).
#'
#' @return A list:
#'   \describe{
#'     \item{probs}{data.table keyed by \code{key = "t1||t2"} (t1 < t2
#'       alphabetically) with \code{p_t1}, \code{p_draw}, \code{p_t2},
#'       \code{lambda_t1}, \code{lambda_t2}.}
#'     \item{lookup}{environment hash: \code{lookup[[key]]} ->
#'       \code{c(p_t1, p_draw, p_t2, lambda_t1, lambda_t2)} for O(1) access.}
#'     \item{team_elo}{named numeric vector of each team's pre-tournament Elo.}
#'   }
#' @family world cup simulation
#' @export
build_knockout_lookup <- function(match_dataset, goals_models, outcome_result,
                                    season = "2026 Canada-Mexico-USA",
                                    hosts = c("United States", "Canada", "Mexico"),
                                    verbose = TRUE) {
  dt <- as.data.frame(match_dataset, stringsAsFactors = FALSE)
  feature_cols       <- goals_models$feature_cols
  augmented_features <- outcome_result$augmented_features
  # WC matchups are international: blend the pooled and international models.
  gm_p <- goals_models$pooled;        om_p <- outcome_result$pooled
  gm_i <- goals_models$international;  om_i <- outcome_result$international
  if (is.null(gm_p) || is.null(gm_i) || is.null(om_p) || is.null(om_i)) {
    cli::cli_abort("build_knockout_lookup: expected $pooled and $international sub-models")
  }
  w_blend <- MATCH_INTL_BLEND_WEIGHT
  blend_ko <- function(p, i) list(
    hg = (1 - w_blend) * p$hg + w_blend * i$hg,
    ag = (1 - w_blend) * p$ag + w_blend * i$ag,
    pH = (1 - w_blend) * p$pH + w_blend * i$pH,
    pD = (1 - w_blend) * p$pD + w_blend * i$pD,
    pA = (1 - w_blend) * p$pA + w_blend * i$pA)

  wc <- dt[dt$league == "WC" & dt$season == season &
             dt$home_team != "" & dt$away_team != "", ]
  if (nrow(wc) == 0) cli::cli_abort("build_knockout_lookup: no WC rows for season {season}")
  teams <- sort(unique(c(wc$home_team, wc$away_team)))

  # Paired home_/away_ columns only (these are team properties). home_field
  # has no away_ partner -- it is a match-level feature, handled separately.
  home_cols <- grep("^home_", feature_cols, value = TRUE)
  home_cols <- home_cols[paste0("away_", sub("^home_", "", home_cols)) %in%
                           feature_cols]
  bases     <- sub("^home_", "", home_cols)

  # --- per-team feature block (base-named) -------------------------------
  # A team's home_X value equals its away_X value (both are the team's own
  # aggregate). Extract from a home row; fall back to an away row.
  team_block <- list()
  for (tm in teams) {
    hr <- wc[wc$home_team == tm, ]
    if (nrow(hr) > 0) {
      blk <- vapply(home_cols, function(c) as.numeric(hr[[c]][1]), numeric(1))
    } else {
      ar <- wc[wc$away_team == tm, ]
      blk <- vapply(paste0("away_", bases),
                    function(c) as.numeric(ar[[c]][1]), numeric(1))
    }
    names(blk) <- bases
    team_block[[tm]] <- blk
  }
  team_elo <- vapply(teams, function(tm) unname(team_block[[tm]]["elo"]),
                     numeric(1))
  names(team_elo) <- teams

  # --- diff-column -> team-feature-base mapping --------------------------
  # Every *_diff / diff_* column is home_<base> - away_<base> (see the diff
  # construction in R/match_prediction.R). The `diff_*` rolling differentials
  # follow a generic rule; the `*_diff` summary differentials use an explicit
  # map. (Empirical discovery is unreliable here because internationals have
  # constant-fill rolling features, making many home-away gaps degenerate.)
  suffix_base <- c(
    panna_diff = "sum_panna",     offense_diff = "sum_offense",
    defense_diff = "sum_defense", spm_diff = "sum_spm",
    epr_diff = "sum_epr",         epr_off_diff = "sum_epr_off",
    epr_def_diff = "sum_epr_def", psr_diff = "sum_psr",
    osr_diff = "sum_osr",         dsr_diff = "sum_dsr",
    elo_diff = "elo",             rest_diff = "days_since_last",
    wpa_diff = "sum_wpa",         psv_diff = "sum_psv",
    centrality_diff = "avg_centrality",
    sk_att_diff = "sk_att_composite", sk_def_diff = "sk_def_composite")
  diff_cols <- grep("(_diff$|^diff_)", feature_cols, value = TRUE)
  diff_base <- character(0)
  for (d in diff_cols) {
    base <- if (startsWith(d, "diff_")) sub("^diff_", "", d) else suffix_base[[d]]
    if (is.null(base) || is.na(base) || !base %in% bases) {
      cli::cli_abort(c(
        "build_knockout_lookup: cannot resolve diff column {.field {d}}",
        "i" = "resolved base {.val {base}} is not a team feature"))
    }
    diff_base[d] <- base
  }

  # --- assemble all 1128 matchup rows (t1 = alphabetically first) --------
  pairs <- t(utils::combn(teams, 2))
  np <- nrow(pairs)
  X <- matrix(0, nrow = np, ncol = length(feature_cols),
              dimnames = list(NULL, feature_cols))
  # Match-level features (league dummies, is_early_season, ...) from a real
  # WC row template; only venue + month vary per knockout matchup.
  for (cn in feature_cols) X[, cn] <- as.numeric(wc[[cn]][1])

  hf <- integer(np)
  for (i in seq_len(np)) {
    b1 <- team_block[[pairs[i, 1]]]
    b2 <- team_block[[pairs[i, 2]]]
    X[i, home_cols]              <- b1[bases]
    X[i, paste0("away_", bases)] <- b2[bases]
    for (d in diff_cols) X[i, d] <- b1[diff_base[d]] - b2[diff_base[d]]
    t1h <- pairs[i, 1] %in% hosts
    t2h <- pairs[i, 2] %in% hosts
    hf[i] <- if (t1h && !t2h) 1L else if (t2h && !t1h) -1L else 0L
  }
  if ("home_field" %in% feature_cols)       X[, "home_field"] <- hf
  if ("is_neutral_venue" %in% feature_cols) X[, "is_neutral_venue"] <- as.integer(hf == 0L)
  if ("match_month" %in% feature_cols)      X[, "match_month"] <- 7   # knockouts: July

  # --- predict both orientations, average (orientation-invariant) --------
  orig <- blend_ko(.ko_predict(X, gm_p, om_p, augmented_features),
                   .ko_predict(X, gm_i, om_i, augmented_features))
  Xm <- X
  for (bs in bases) {
    h <- paste0("home_", bs); a <- paste0("away_", bs)
    tmp <- Xm[, h]; Xm[, h] <- Xm[, a]; Xm[, a] <- tmp
  }
  for (d in diff_cols) Xm[, d] <- -Xm[, d]
  if ("home_field" %in% feature_cols) Xm[, "home_field"] <- -Xm[, "home_field"]
  mir <- blend_ko(.ko_predict(Xm, gm_p, om_p, augmented_features),
                  .ko_predict(Xm, gm_i, om_i, augmented_features))

  probs <- data.table::data.table(
    t1        = pairs[, 1],
    t2        = pairs[, 2],
    p_t1      = (orig$pH + mir$pA) / 2,
    p_draw    = (orig$pD + mir$pD) / 2,
    p_t2      = (orig$pA + mir$pH) / 2,
    lambda_t1 = (orig$hg + mir$ag) / 2,
    lambda_t2 = (orig$ag + mir$hg) / 2
  )
  probs[, key := paste(t1, t2, sep = "||")]

  # Environment hash for O(1) lookup inside the sim loop.
  lookup <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(probs))) {
    lookup[[probs$key[i]]] <- c(probs$p_t1[i], probs$p_draw[i], probs$p_t2[i],
                                 probs$lambda_t1[i], probs$lambda_t2[i])
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "Knockout lookup: {np} matchups predicted with the full model ({length(feature_cols)} features)")
  }
  list(probs = probs, lookup = lookup, team_elo = team_elo)
}
