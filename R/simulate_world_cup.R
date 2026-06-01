# 48-team World Cup tournament simulator (2026 format)
#
# Group stage: 12 groups of 4, top 2 + 8 best 3rd-placed -> Round of 32
# Knockouts: R32 -> R16 -> QF -> SF -> Final
#
# Match probabilities:
#   * Group stage  -- the cached full-model predictions for each known fixture.
#   * Knockouts    -- the full-model pairwise lookup (build_knockout_lookup).
#
# "Run hot" momentum: each team carries a dynamic Elo through the tournament,
# initialised from its pre-tournament Elo. After every simulated game both
# teams' Elo updates from the result; before each game the base probability is
# nudged by the Elo drift so far. The `elo_k` parameter sets the strength.
#
# Performance: teams are referenced by integer index throughout the hot loop
# (no name hashing); per-sim randomness is drawn in blocks; the knockout
# lookup is an integer-indexed matrix. ~3-5x faster than the name-keyed
# scalar-RNG version, same algorithm.

#' Simulate a 48-team World Cup
#'
#' @param predictions Data frame with `home_team`, `away_team`, `prob_H`,
#'   `prob_D`, `prob_A`, `pred_home_goals`, `pred_away_goals` for all
#'   group-stage fixtures.
#' @param groups Data frame mapping `team` -> `group` (12 groups of 4).
#' @param knockout Output of [build_knockout_lookup()] -- a list with `probs`
#'   (pairwise knockout probabilities) and `team_elo` (named vector of
#'   pre-tournament Elo, used as the run-hot baseline).
#' @param n_sims Integer. Default 10000.
#' @param elo_k Run-hot Elo K-factor (default 20; 0 disables momentum).
#' @param verbose Logical. Print progress. Default TRUE.
#'
#' @return A list with `summary` (per-team round probabilities),
#'   `group_table` (group-position probabilities), `n_sims`, `elo_k`.
#' @export
simulate_world_cup <- function(predictions, groups, knockout,
                                n_sims = 10000L,
                                elo_k = 40,
                                verbose = TRUE) {

  ## --- 1. Teams + integer indexing ---------------------------------------
  pred <- as.data.frame(predictions, stringsAsFactors = FALSE)
  pred <- pred[pred$home_team != "" & pred$away_team != "", ]
  pred$key <- pmap_chr2(pred$home_team, pred$away_team)

  groups <- as.data.frame(groups, stringsAsFactors = FALSE)
  all_teams <- groups$team
  n_teams   <- length(all_teams)
  team_group <- stats::setNames(groups$group, groups$team)
  group_letters <- sort(unique(groups$group))
  n_groups <- length(group_letters)

  ## Pre-tournament Elo as a plain numeric vector, integer-indexed.
  elo_base <- numeric(n_teams)
  med_elo  <- stats::median(knockout$team_elo, na.rm = TRUE)
  for (k in seq_len(n_teams)) {
    e <- knockout$team_elo[[all_teams[k]]]
    elo_base[k] <- if (is.null(e) || is.na(e)) med_elo else e
  }

  ## --- 2. Knockout probability matrices (integer-indexed) ----------------
  ## WIN[i, j]  = P(team i beats team j);  DRAW[i, j] = P(draw);
  ## LAM[i, j]  = expected goals for team i against team j.
  WIN  <- matrix(NA_real_, n_teams, n_teams)
  DRAW <- matrix(NA_real_, n_teams, n_teams)
  LAM  <- matrix(NA_real_, n_teams, n_teams)
  kp <- as.data.frame(knockout$probs, stringsAsFactors = FALSE)
  ti <- match(kp$t1, all_teams)
  tj <- match(kp$t2, all_teams)
  ok <- !is.na(ti) & !is.na(tj)
  WIN[cbind(ti[ok], tj[ok])]  <- kp$p_t1[ok]
  WIN[cbind(tj[ok], ti[ok])]  <- kp$p_t2[ok]
  DRAW[cbind(ti[ok], tj[ok])] <- kp$p_draw[ok]
  DRAW[cbind(tj[ok], ti[ok])] <- kp$p_draw[ok]
  LAM[cbind(ti[ok], tj[ok])]  <- kp$lambda_t1[ok]
  LAM[cbind(tj[ok], ti[ok])]  <- kp$lambda_t2[ok]

  ## --- 3. Group fixtures as flat integer-indexed vectors -----------------
  ## All 72 group matches, ordered group-by-group so processing them in order
  ## preserves within-group sequence (needed for run-hot momentum).
  group_team_glob <- vector("list", n_groups)
  n_gm <- n_groups * choose(4L, 2L)            # 12 * 6 = 72
  m_grp  <- integer(n_gm)
  m_t1g  <- integer(n_gm); m_t2g <- integer(n_gm)
  m_p1   <- numeric(n_gm); m_pd  <- numeric(n_gm); m_p2 <- numeric(n_gm)
  m_lam1 <- numeric(n_gm); m_lam2 <- numeric(n_gm)
  mc <- 0L
  for (gi in seq_len(n_groups)) {
    teams_g <- sort(groups$team[groups$group == group_letters[gi]])
    group_team_glob[[gi]] <- match(teams_g, all_teams)
    pairs <- utils::combn(teams_g, 2)
    for (c in seq_len(ncol(pairs))) {
      t1 <- pairs[1, c]; t2 <- pairs[2, c]
      i <- match(t1, all_teams); j <- match(t2, all_teams)
      r <- pred[pred$key == paste(t1, t2, sep = "||"), ][1, , drop = FALSE]
      if (nrow(r) == 0 || is.na(r$prob_H)) {
        p1 <- WIN[i, j]; pd <- DRAW[i, j]; p2 <- WIN[j, i]
        l1 <- LAM[i, j]; l2 <- LAM[j, i]
      } else if (r$home_team == t1) {
        p1 <- r$prob_H; pd <- r$prob_D; p2 <- r$prob_A
        l1 <- r$pred_home_goals; l2 <- r$pred_away_goals
      } else {
        p1 <- r$prob_A; pd <- r$prob_D; p2 <- r$prob_H
        l1 <- r$pred_away_goals; l2 <- r$pred_home_goals
      }
      mc <- mc + 1L
      m_grp[mc] <- gi; m_t1g[mc] <- i; m_t2g[mc] <- j
      m_p1[mc] <- p1; m_pd[mc] <- pd; m_p2[mc] <- p2
      m_lam1[mc] <- max(0.2, l1); m_lam2[mc] <- max(0.2, l2)
    }
  }

  ## --- 4. Counters (integer-indexed, rows align with all_teams) ----------
  reach      <- matrix(0L, nrow = n_teams, ncol = 6)  # R32 R16 QF SF F Champ
  pos_counts <- matrix(0L, nrow = n_teams, ncol = 4)

  if (verbose) {
    cli::cli_alert_info(
      "Simulating {n_sims} World Cups (run-hot elo_k = {elo_k})...")
  }
  pb_every <- max(1L, n_sims %/% 20L)

  ## --- 5. Simulation loop ------------------------------------------------
  for (sim in seq_len(n_sims)) {
    if (verbose && sim %% pb_every == 0L) {
      cli::cli_alert_info("  sim {sim}/{n_sims}")
    }

    elo_dyn <- elo_base
    pts <- integer(n_teams); gf <- integer(n_teams); ga <- integer(n_teams)

    ## Group-stage randomness, drawn in blocks.
    u_g    <- stats::runif(n_gm)
    goals1 <- pmin(stats::rpois(n_gm, m_lam1), 8L)
    goals2 <- pmin(stats::rpois(n_gm, m_lam2), 8L)
    tb         <- stats::runif(n_teams)     # group tiebreaks (4 per group)
    third_rand <- stats::runif(n_groups)

    ## --- 5a. Group stage (72 matches, group-by-group order) --------------
    for (mi in seq_len(n_gm)) {
      t1 <- m_t1g[mi]; t2 <- m_t2g[mi]
      drift <- (elo_dyn[t1] - elo_base[t1]) - (elo_dyn[t2] - elo_base[t2])
      ps <- elo_shift_probs(m_p1[mi], m_pd[mi], m_p2[mi], drift)

      g1 <- goals1[mi]; g2 <- goals2[mi]; u <- u_g[mi]
      if (u < ps[1]) {
        if (g1 <= g2) g1 <- g2 + 1L
        pts[t1] <- pts[t1] + 3L
      } else if (u < ps[1] + ps[2]) {
        if (g1 != g2) g2 <- g1
        pts[t1] <- pts[t1] + 1L
        pts[t2] <- pts[t2] + 1L
      } else {
        if (g2 <= g1) g2 <- g1 + 1L
        pts[t2] <- pts[t2] + 3L
      }
      gf[t1] <- gf[t1] + g1; ga[t1] <- ga[t1] + g2
      gf[t2] <- gf[t2] + g2; ga[t2] <- ga[t2] + g1

      eu <- elo_update_pair(elo_dyn[t1], elo_dyn[t2], g1, g2, elo_k)
      elo_dyn[t1] <- eu[1]; elo_dyn[t2] <- eu[2]
    }

    ## --- 5b. Rank groups, collect advancers + third-place pool -----------
    advancers_top2 <- integer(2L * n_groups)
    third_glob <- integer(n_groups); third_pts <- integer(n_groups)
    third_gd   <- integer(n_groups); third_gf  <- integer(n_groups)
    for (gi in seq_len(n_groups)) {
      gti <- group_team_glob[[gi]]
      p <- pts[gti]; d <- gf[gti] - ga[gti]; f <- gf[gti]
      tbk <- tb[((gi - 1L) * 4L + 1L):((gi - 1L) * 4L + 4L)]
      ord <- order(-p, -d, -f, tbk)
      ranked <- gti[ord]
      pos_counts[ranked[1], 1] <- pos_counts[ranked[1], 1] + 1L
      pos_counts[ranked[2], 2] <- pos_counts[ranked[2], 2] + 1L
      pos_counts[ranked[3], 3] <- pos_counts[ranked[3], 3] + 1L
      pos_counts[ranked[4], 4] <- pos_counts[ranked[4], 4] + 1L
      advancers_top2[(2L * gi - 1L):(2L * gi)] <- ranked[1:2]
      third_glob[gi] <- ranked[3]
      third_pts[gi]  <- p[ord[3]]
      third_gd[gi]   <- d[ord[3]]
      third_gf[gi]   <- f[ord[3]]
    }

    ## --- 5c. Best 8 third-placed teams -----------------------------------
    ord3 <- order(-third_pts, -third_gd, -third_gf, third_rand)
    r32  <- c(advancers_top2, third_glob[ord3[1:8]])
    reach[r32, 1] <- reach[r32, 1] + 1L

    ## --- 5d. Knockouts ---------------------------------------------------
    ## Random reseeding; momentum carries forward via elo_dyn.
    bracket <- sample(r32)
    for (rd in 2:6) {
      kr <- play_knockout_round(bracket, WIN, DRAW, LAM,
                                elo_dyn, elo_base, elo_k)
      bracket <- kr$winners
      elo_dyn <- kr$elo_dyn
      reach[bracket, rd] <- reach[bracket, rd] + 1L
    }
  }

  ## --- 6. Summarise ------------------------------------------------------
  summary_df <- data.frame(
    team    = all_teams,
    group   = unname(team_group[all_teams]),
    p_R16   = round(reach[, 2] / n_sims * 100, 1),
    p_QF    = round(reach[, 3] / n_sims * 100, 1),
    p_SF    = round(reach[, 4] / n_sims * 100, 1),
    p_final = round(reach[, 5] / n_sims * 100, 1),
    p_champ = round(reach[, 6] / n_sims * 100, 2),
    stringsAsFactors = FALSE
  )
  summary_df <- summary_df[order(-summary_df$p_champ, -summary_df$p_final), ]
  rownames(summary_df) <- NULL

  group_table <- data.frame(
    group = unname(team_group[all_teams]),
    team  = all_teams,
    pos1  = pos_counts[, 1] / n_sims * 100,
    pos2  = pos_counts[, 2] / n_sims * 100,
    pos3  = pos_counts[, 3] / n_sims * 100,
    pos4  = pos_counts[, 4] / n_sims * 100,
    stringsAsFactors = FALSE
  )
  group_table <- group_table[order(group_table$group, -group_table$pos1), ]
  rownames(group_table) <- NULL

  list(summary = summary_df, group_table = group_table,
       n_sims = n_sims, elo_k = elo_k)
}


#' Play one knockout round (internal helper)
#'
#' Resolves all ties in a round from the integer-indexed knockout matrices,
#' applies the run-hot Elo nudge, simulates goals for the margin, and updates
#' both teams' dynamic Elo. Round-level randomness is drawn in blocks. A
#' 90-minute draw is decided by a coin flip (extra time / penalties) but
#' counts as a draw for the Elo update.
#' @keywords internal
play_knockout_round <- function(bracket, WIN, DRAW, LAM,
                                 elo_dyn, elo_base, elo_k) {
  n <- length(bracket)
  K <- n %/% 2L
  a <- bracket[seq.int(1L, n, 2L)]
  b <- bracket[seq.int(2L, n, 2L)]
  ab <- cbind(a, b)
  ba <- cbind(b, a)
  p_a <- WIN[ab]; p_d <- DRAW[ab]; p_b <- WIN[ba]
  lam_a <- LAM[ab]; lam_b <- LAM[ba]

  u    <- stats::runif(K)
  coin <- stats::runif(K)
  g_a  <- pmin(stats::rpois(K, pmax(0.2, lam_a)), 8L)
  g_b  <- pmin(stats::rpois(K, pmax(0.2, lam_b)), 8L)

  winners <- integer(K)
  for (i in seq_len(K)) {
    ta <- a[i]; tb <- b[i]
    drift <- (elo_dyn[ta] - elo_base[ta]) - (elo_dyn[tb] - elo_base[tb])
    ps <- elo_shift_probs(p_a[i], p_d[i], p_b[i], drift)
    g1 <- g_a[i]; g2 <- g_b[i]
    if (u[i] < ps[1]) {
      if (g1 <= g2) g1 <- g2 + 1L
      winners[i] <- ta
    } else if (u[i] < ps[1] + ps[2]) {
      g2 <- g1                                   # draw -> ET/penalties
      # Drawn knockout -> penalty shootout. shootout_win_prob() with equal
      # conversion rates is exactly 0.5 (a fair shootout has no structural
      # first-kicker edge), so this is behaviourally identical to the prior
      # bare coin flip — but it names the mechanism and is the single hook to
      # later pass team-specific conversion rates (taker/keeper quality) for
      # non-50/50 shootout odds. ta took the (notional) first kick.
      p_so <- shootout_win_prob()                # P(ta wins) = 0.5 at equal p
      winners[i] <- if (coin[i] < p_so) ta else tb
    } else {
      if (g2 <= g1) g2 <- g1 + 1L
      winners[i] <- tb
    }
    eu <- elo_update_pair(elo_dyn[ta], elo_dyn[tb], g1, g2, elo_k)
    elo_dyn[ta] <- eu[1]; elo_dyn[tb] <- eu[2]
  }
  list(winners = winners, elo_dyn = elo_dyn)
}


#' Nudge match probabilities by a run-hot Elo drift (internal helper)
#'
#' Shifts the win/loss split by `drift` Elo points (logit shift) while leaving
#' the draw probability unchanged. `drift = 0` returns the input untouched.
#' @keywords internal
elo_shift_probs <- function(p1, pd, p2, drift) {
  s <- p1 + p2
  if (drift == 0 || s <= 0) return(c(p1, pd, p2))
  shift <- drift / 400 * log(10)                 # Elo points -> natural logit
  l <- log(max(p1, 1e-9) / max(p2, 1e-9)) + shift
  q <- 1 / (1 + exp(-l))
  c(s * q, pd, s * (1 - q))
}


#' Update a pair of dynamic Elo ratings from a result (internal helper)
#'
#' Standard zero-sum Elo update with a World-Football-Elo goal-difference
#' multiplier. `K = 0` returns the ratings unchanged.
#' @keywords internal
elo_update_pair <- function(e1, e2, g1, g2, K) {
  if (K == 0) return(c(e1, e2))
  expected1 <- 1 / (1 + 10^((e2 - e1) / 400))
  s1 <- if (g1 > g2) 1 else if (g1 == g2) 0.5 else 0
  gd <- abs(g1 - g2)
  gmult <- if (gd <= 1) 1 else if (gd == 2) 1.5 else (11 + gd) / 8
  delta <- K * gmult * (s1 - expected1)
  c(e1 + delta, e2 - delta)
}


#' Helper: sorted-pair key for a fixture
#' @keywords internal
pmap_chr2 <- function(a, b) {
  ifelse(a < b, paste(a, b, sep = "||"), paste(b, a, sep = "||"))
}
