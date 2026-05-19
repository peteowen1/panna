# 48-team World Cup tournament simulator (2026 format)
#
# Group stage: 12 groups of 4, top 2 + 8 best 3rd-placed -> Round of 32
# Knockouts: R32 -> R16 -> QF -> SF -> Final (+ 3rd place playoff)
#
# Group-stage match probabilities come from the cached prediction pipeline.
# Knockout matchups are TBD until the bracket fills in, so we use the
# Bradley-Terry team ratings (fit on those same predictions) to compute
# match probabilities on the fly. Draws in knockouts are resolved by
# coin-flip (extra time / penalties).

#' Simulate a 48-team World Cup
#'
#' @param predictions Data frame with `home_team`, `away_team`, `prob_H`,
#'   `prob_D`, `prob_A`, `pred_home_goals`, `pred_away_goals` for all
#'   group-stage fixtures (rows where both teams are known).
#' @param groups Data frame mapping `team` -> `group` (12 groups, 4 teams each).
#' @param bt_ratings Output of `fit_bt_ratings(predictions, neutral = TRUE)`.
#' @param n_sims Integer. Default 10000.
#' @param verbose Logical. Print progress. Default TRUE.
#'
#' @return A list with:
#'   \itemize{
#'     \item `summary`: per-team probabilities of reaching each round
#'     \item `group_table`: expected group standings (final position probs)
#'     \item `n_sims`: simulation count
#'   }
#' @export
simulate_world_cup <- function(predictions, groups, bt_ratings,
                                n_sims = 10000L,
                                verbose = TRUE) {

  ## --- 1. Build lookup tables ---------------------------------------------
  pred <- as.data.frame(predictions, stringsAsFactors = FALSE)
  pred <- pred[pred$home_team != "" & pred$away_team != "", ]
  ## key by sorted (team1, team2) so we can find a fixture's probs regardless
  ## of which side is "home" in opta (WC is neutral venue anyway)
  pred$key <- pmap_chr2(pred$home_team, pred$away_team)
  pred$home_is_first <- pred$home_team < pred$away_team

  groups <- as.data.frame(groups, stringsAsFactors = FALSE)
  team_group <- stats::setNames(groups$group, groups$team)

  ratings  <- bt_ratings$ratings
  team_idx <- stats::setNames(seq_len(nrow(ratings)), ratings$team)
  r_vec    <- ratings$rating
  nu       <- bt_ratings$nu

  ## --- 2. Pre-extract per-group fixture probabilities ---------------------
  ## For each group, list of 6 matches with cum_H, cum_D, lambda_home, lambda_away
  group_letters <- sort(unique(groups$group))
  group_fixtures <- vector("list", length(group_letters))
  names(group_fixtures) <- group_letters
  for (g in group_letters) {
    teams_g <- groups$team[groups$group == g]
    pairs <- utils::combn(sort(teams_g), 2)
    rows  <- lapply(seq_len(ncol(pairs)), function(i) {
      key <- paste(pairs[1, i], pairs[2, i], sep = "||")
      r <- pred[pred$key == key, ][1, , drop = FALSE]
      if (nrow(r) == 0 || is.na(r$prob_H)) {
        ## Fallback: use BT ratings (neutral venue)
        p <- bt_match_prob(r_vec[team_idx[pairs[1, i]]],
                           r_vec[team_idx[pairs[2, i]]],
                           home_adv = 0, nu = nu)
        list(t1 = pairs[1, i], t2 = pairs[2, i],
             p_t1_win = p["prob_H"], p_draw = p["prob_D"], p_t2_win = p["prob_A"],
             lambda_t1 = 1.2, lambda_t2 = 1.2)
      } else {
        ## Orient probs so t1 corresponds to alphabetically-first team
        if (r$home_team == pairs[1, i]) {
          list(t1 = pairs[1, i], t2 = pairs[2, i],
               p_t1_win = r$prob_H, p_draw = r$prob_D, p_t2_win = r$prob_A,
               lambda_t1 = r$pred_home_goals, lambda_t2 = r$pred_away_goals)
        } else {
          list(t1 = pairs[1, i], t2 = pairs[2, i],
               p_t1_win = r$prob_A, p_draw = r$prob_D, p_t2_win = r$prob_H,
               lambda_t1 = r$pred_away_goals, lambda_t2 = r$pred_home_goals)
        }
      }
    })
    group_fixtures[[g]] <- rows
  }

  ## --- 3. Counters --------------------------------------------------------
  all_teams <- groups$team
  n_teams   <- length(all_teams)
  reach <- matrix(0L, nrow = n_teams, ncol = 6,
                  dimnames = list(all_teams,
                                  c("R32", "R16", "QF", "SF", "F", "Champ")))
  ## final group position (1-4) counts per team
  pos_counts <- matrix(0L, nrow = n_teams, ncol = 4,
                       dimnames = list(all_teams, paste0("pos", 1:4)))

  if (verbose) cli::cli_alert_info("Simulating {n_sims} World Cups...")
  pb_every <- max(1L, n_sims %/% 20L)

  ## --- 4. Simulation loop -------------------------------------------------
  for (sim in seq_len(n_sims)) {
    if (verbose && sim %% pb_every == 0L) {
      cli::cli_alert_info("  sim {sim}/{n_sims}")
    }

    ## --- 4a. Group stage --------------------------------------------------
    advancers_top2  <- character(0)  # group winner/runner-up (24 teams)
    third_place_pool <- list()       # for each group: 3rd-placed team info

    for (g in group_letters) {
      teams_g <- groups$team[groups$group == g]
      pts <- stats::setNames(integer(length(teams_g)), teams_g)
      gf  <- stats::setNames(integer(length(teams_g)), teams_g)
      ga  <- stats::setNames(integer(length(teams_g)), teams_g)
      h2h <- list()  # head-to-head: list of (winner, loser, draw)

      for (m in group_fixtures[[g]]) {
        u <- stats::runif(1)
        ## Goals via Poisson (capped to keep within Opta plausibility)
        g1 <- min(stats::rpois(1, max(0.2, m$lambda_t1)), 8L)
        g2 <- min(stats::rpois(1, max(0.2, m$lambda_t2)), 8L)

        if (u < m$p_t1_win) {
          ## ensure goals consistent with outcome (resample if needed)
          if (g1 <= g2) { g1 <- g2 + 1L }
          pts[m$t1] <- pts[m$t1] + 3L
        } else if (u < m$p_t1_win + m$p_draw) {
          if (g1 != g2) { g2 <- g1 }
          pts[m$t1] <- pts[m$t1] + 1L
          pts[m$t2] <- pts[m$t2] + 1L
        } else {
          if (g2 <= g1) { g2 <- g1 + 1L }
          pts[m$t2] <- pts[m$t2] + 3L
        }
        gf[m$t1] <- gf[m$t1] + g1; ga[m$t1] <- ga[m$t1] + g2
        gf[m$t2] <- gf[m$t2] + g2; ga[m$t2] <- ga[m$t2] + g1
      }

      ## Rank within group: pts, GD, GF, random tiebreak
      gd <- gf - ga
      tiebreak <- stats::runif(length(teams_g))
      ord <- order(-pts, -gd, -gf, tiebreak)
      ranked <- teams_g[ord]
      for (i in seq_along(ranked)) {
        pos_counts[ranked[i], i] <- pos_counts[ranked[i], i] + 1L
      }

      advancers_top2 <- c(advancers_top2, ranked[1:2])
      third_place_pool[[g]] <- list(
        team = ranked[3],
        pts  = pts[ranked[3]],
        gd   = gd[ranked[3]],
        gf   = gf[ranked[3]],
        rand = stats::runif(1)
      )
    }

    ## --- 4b. Best 8 third-placed teams ------------------------------------
    tp <- do.call(rbind, lapply(third_place_pool, function(x)
      data.frame(team = x$team, pts = x$pts, gd = x$gd, gf = x$gf, rand = x$rand,
                 stringsAsFactors = FALSE)))
    tp <- tp[order(-tp$pts, -tp$gd, -tp$gf, tp$rand), ]
    advancers_third <- tp$team[1:8]

    r32_teams <- c(advancers_top2, advancers_third)
    reach[r32_teams, "R32"] <- reach[r32_teams, "R32"] + 1L

    ## --- 4c. Knockouts: random reseeding ----------------------------------
    ## (See package note: official 2026 bracket isn't published in our data,
    ## random reseeding gives unbiased champion probabilities.)
    bracket <- sample(r32_teams)
    bracket <- play_knockout_round(bracket, r_vec, team_idx, nu)
    reach[bracket, "R16"] <- reach[bracket, "R16"] + 1L
    bracket <- play_knockout_round(bracket, r_vec, team_idx, nu)
    reach[bracket, "QF"]  <- reach[bracket, "QF"]  + 1L
    bracket <- play_knockout_round(bracket, r_vec, team_idx, nu)
    reach[bracket, "SF"]  <- reach[bracket, "SF"]  + 1L
    bracket <- play_knockout_round(bracket, r_vec, team_idx, nu)
    reach[bracket, "F"]   <- reach[bracket, "F"]   + 1L
    bracket <- play_knockout_round(bracket, r_vec, team_idx, nu)
    reach[bracket, "Champ"] <- reach[bracket, "Champ"] + 1L
  }

  ## --- 5. Summarise -------------------------------------------------------
  summary_df <- data.frame(
    team = all_teams,
    group = unname(team_group[all_teams]),
    p_R16   = round(reach[, "R16"]   / n_sims * 100, 1),
    p_QF    = round(reach[, "QF"]    / n_sims * 100, 1),
    p_SF    = round(reach[, "SF"]    / n_sims * 100, 1),
    p_final = round(reach[, "F"]     / n_sims * 100, 1),
    p_champ = round(reach[, "Champ"] / n_sims * 100, 2),
    stringsAsFactors = FALSE
  )
  summary_df <- summary_df[order(-summary_df$p_champ, -summary_df$p_final), ]
  rownames(summary_df) <- NULL

  group_table <- as.data.frame(pos_counts / n_sims * 100)
  group_table$team  <- rownames(pos_counts)
  group_table$group <- unname(team_group[group_table$team])
  group_table <- group_table[, c("group", "team", "pos1", "pos2", "pos3", "pos4")]
  group_table <- group_table[order(group_table$group, -group_table$pos1), ]
  rownames(group_table) <- NULL

  list(summary = summary_df, group_table = group_table, n_sims = n_sims)
}

#' Play one round of a knockout bracket (internal helper)
#' @keywords internal
play_knockout_round <- function(bracket, r_vec, team_idx, nu) {
  n <- length(bracket)
  winners <- character(n / 2)
  for (i in seq_len(n / 2)) {
    t1 <- bracket[2 * i - 1]
    t2 <- bracket[2 * i]
    p <- bt_match_prob(r_vec[team_idx[t1]], r_vec[team_idx[t2]],
                       home_adv = 0, nu = nu)
    ## Draws resolved by coin flip (extra time / penalties)
    p_t1 <- p["prob_H"] + 0.5 * p["prob_D"]
    winners[i] <- if (stats::runif(1) < p_t1) t1 else t2
  }
  winners
}

#' Helper: sorted-pair key for a fixture
#' @keywords internal
pmap_chr2 <- function(a, b) {
  ifelse(a < b, paste(a, b, sep = "||"), paste(b, a, sep = "||"))
}
