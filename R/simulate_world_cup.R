# 48-team World Cup tournament simulator (2026 format)
#
# Group stage: 12 groups of 4, top 2 + 8 best 3rd-placed -> Round of 32
# Knockouts: R32 -> R16 -> QF -> SF -> Final, on the OFFICIAL FIFA 2026
# bracket (matches 73-104): fixed slots for group winners/runners-up, the
# eight best thirds allocated to their designated winner-slots subject to
# FIFA's per-slot group-eligibility lists.
#
# Match probabilities:
#   * Group stage  -- the cached full-model predictions for each known fixture.
#   * Knockouts    -- the full-model pairwise lookup (build_knockout_lookup).
#
# Group ranking follows FIFA tiebreakers as far as the model can: points,
# goal difference, goals for, then head-to-head record (points/GD/GF among
# the tied teams), then random (standing in for fair-play points and the
# drawing of lots, which the model has no signal for). Third-place ranking
# is points/GD/GF then random (FIFA: disciplinary points, then lots).
#
# "Run hot" momentum: each team carries a dynamic Elo through the tournament,
# initialised from its pre-tournament Elo. After every simulated game both
# teams' Elo updates from the result; before each game the base probability is
# nudged by the Elo drift so far. The `elo_k` parameter sets the strength.
#
# Performance: teams are referenced by integer index throughout the hot loop
# (no name hashing); per-sim randomness is drawn in blocks; the knockout
# lookup is an integer-indexed matrix; the third-place slot allocation is a
# precomputed 495-row lookup keyed by the bitmask of qualified groups.

## --- FIFA 2026 bracket constants -------------------------------------------
## Round of 32 ties in match order 73..88 (Wikipedia, "2026 FIFA World Cup
## knockout stage", verified 2026-06-11). Sides: W = winner of group,
## R = runner-up of group, T = best-third slot with its eligible groups.
##
##   M73 R-A v R-B    M77 W-I v 3rd(CDFGH)  M81 W-D v 3rd(BEFIJ)  M85 W-B v 3rd(EFGIJ)
##   M74 W-E v 3rd(ABCDF) M78 R-E v R-I     M82 W-G v 3rd(AEHIJ)  M86 W-J v R-H
##   M75 W-F v R-C    M79 W-A v 3rd(CEFHI)  M83 R-K v R-L         M87 W-K v 3rd(DEIJL)
##   M76 W-C v R-F    M80 W-L v 3rd(EHIJK)  M84 W-H v R-J         M88 R-D v R-G
##
## Later rounds (positions into the previous round's winners vector, winners
## kept in match order): R16 M89..M96, QF M97..M100, SF M101/M102, Final M104.
wc2026_bracket_spec <- function() {
  tie <- function(a_type, a_key, b_type, b_key) {
    list(a = c(a_type, a_key), b = c(b_type, b_key))
  }
  list(
    r32 = list(
      tie("R", "A", "R", "B"),        # M73
      tie("W", "E", "T", "ABCDF"),    # M74
      tie("W", "F", "R", "C"),        # M75
      tie("W", "C", "R", "F"),        # M76
      tie("W", "I", "T", "CDFGH"),    # M77
      tie("R", "E", "R", "I"),        # M78
      tie("W", "A", "T", "CEFHI"),    # M79
      tie("W", "L", "T", "EHIJK"),    # M80
      tie("W", "D", "T", "BEFIJ"),    # M81
      tie("W", "G", "T", "AEHIJ"),    # M82
      tie("R", "K", "R", "L"),        # M83
      tie("W", "H", "R", "J"),        # M84
      tie("W", "B", "T", "EFGIJ"),    # M85
      tie("W", "J", "R", "H"),        # M86
      tie("W", "K", "T", "DEIJL"),    # M87
      tie("R", "D", "R", "G")         # M88
    ),
    ## M89 = W74 v W77, M90 = W73 v W75, M91 = W76 v W78, M92 = W79 v W80,
    ## M93 = W83 v W84, M94 = W81 v W82, M95 = W86 v W88, M96 = W85 v W87
    r16 = c(2L, 5L, 1L, 3L, 4L, 6L, 7L, 8L, 11L, 12L, 9L, 10L, 14L, 16L, 13L, 15L),
    ## M97 = W89 v W90, M98 = W93 v W94, M99 = W91 v W92, M100 = W95 v W96
    qf  = c(1L, 2L, 5L, 6L, 3L, 4L, 7L, 8L),
    ## M101 = W97 v W98, M102 = W99 v W100
    sf  = c(1L, 2L, 3L, 4L),
    fin = c(1L, 2L)
  )
}

#' Precompute the third-place slot allocation lookup (internal)
#'
#' For every C(12, 8) = 495 combination of groups whose third-placed team
#' qualifies, finds an assignment of those 8 groups to the 8 designated
#' R32 third-place slots that respects each slot's eligible-group list
#' (backtracking, most-constrained slot first). FIFA's regulations pin one
#' specific assignment per combination in an annex table; any
#' eligibility-respecting assignment has the same bracket geometry up to
#' which allowed third a winner draws, so this canonical solution is used
#' as a close approximation of the official table.
#'
#' @param slot_cands List of integer vectors (group indices 1..12), the
#'   eligible groups per third slot, in R32 match order.
#' @return Integer matrix with 2^12 rows (indexed by bitmask of qualified
#'   groups + 1) and one column per slot; valid rows hold the assigned
#'   group index per slot, all other rows are NA.
#' @keywords internal
build_third_allocation <- function(slot_cands) {
  n_slots <- length(slot_cands)
  combos <- utils::combn(12L, 8L)
  alloc <- matrix(NA_integer_, nrow = 2L^12L, ncol = n_slots)
  for (ci in seq_len(ncol(combos))) {
    qual <- combos[, ci]
    cand <- lapply(slot_cands, intersect, qual)
    ord  <- order(lengths(cand))
    assign_vec <- integer(n_slots)
    used <- logical(12L)
    solve_slot <- function(k) {
      if (k > n_slots) return(TRUE)
      s <- ord[k]
      for (g in cand[[s]]) {
        if (!used[g]) {
          used[g] <<- TRUE
          assign_vec[s] <<- g
          if (solve_slot(k + 1L)) return(TRUE)
          used[g] <<- FALSE
        }
      }
      FALSE
    }
    if (!solve_slot(1L)) {
      stop("No valid third-place slot allocation exists for qualified groups: ",
           paste(LETTERS[qual], collapse = ", "))
    }
    alloc[sum(2L^(qual - 1L)) + 1L, ] <- assign_vec
  }
  alloc
}

#' Rank one group with FIFA tiebreakers (internal)
#'
#' Orders the four teams of a group by points, goal difference, goals for,
#' then -- for teams still tied on all three -- by head-to-head points, GD
#' and GF among the tied teams, then by `tbk` (random, standing in for fair
#' play and drawing of lots).
#'
#' @param p,d,f Length-4 integer vectors: points, goal difference, goals for
#'   (aligned with local team slots 1..4).
#' @param tbk Length-4 numeric tiebreak randoms.
#' @param m_a,m_b Length-6 integer vectors: local team slots per group match.
#' @param g_a,g_b Length-6 integer vectors: final goals per group match.
#' @return Integer permutation of 1:4 (best first).
#' @keywords internal
rank_group_h2h <- function(p, d, f, tbk, m_a, m_b, g_a, g_b) {
  ord <- order(-p, -d, -f, tbk)
  i <- 1L
  while (i < 4L) {
    j <- i
    while (j < 4L &&
           p[ord[j + 1L]] == p[ord[i]] &&
           d[ord[j + 1L]] == d[ord[i]] &&
           f[ord[j + 1L]] == f[ord[i]]) {
      j <- j + 1L
    }
    if (j > i) {
      cluster <- ord[i:j]
      hp <- integer(4L); hd <- integer(4L); hf <- integer(4L)
      for (mi in seq_along(m_a)) {
        a <- m_a[mi]; b <- m_b[mi]
        if (a %in% cluster && b %in% cluster) {
          ga <- g_a[mi]; gb <- g_b[mi]
          if (ga > gb)      hp[a] <- hp[a] + 3L
          else if (ga < gb) hp[b] <- hp[b] + 3L
          else { hp[a] <- hp[a] + 1L; hp[b] <- hp[b] + 1L }
          hd[a] <- hd[a] + ga - gb; hd[b] <- hd[b] + gb - ga
          hf[a] <- hf[a] + ga;      hf[b] <- hf[b] + gb
        }
      }
      ord[i:j] <- cluster[order(-hp[cluster], -hd[cluster], -hf[cluster],
                                tbk[cluster])]
    }
    i <- j + 1L
  }
  ord
}

#' Simulate a 48-team World Cup
#'
#' @param predictions Data frame with `home_team`, `away_team`, `prob_H`,
#'   `prob_D`, `prob_A`, `pred_home_goals`, `pred_away_goals` for all
#'   group-stage fixtures.
#' @param groups Data frame mapping `team` -> `group` (12 groups of 4).
#'   Group letters must match the official draw when
#'   `bracket = "fifa2026"` -- the letters determine knockout paths.
#' @param knockout Output of [build_knockout_lookup()] -- a list with `probs`
#'   (pairwise knockout probabilities) and `team_elo` (named vector of
#'   pre-tournament Elo, used as the run-hot baseline).
#' @param n_sims Integer. Default 10000.
#' @param elo_k Run-hot Elo K-factor (default 20; 0 disables momentum).
#' @param bracket `"fifa2026"` (default) plays the knockouts on the official
#'   2026 bracket (matches 73-104) with FIFA's third-place slot eligibility;
#'   `"random"` reshuffles the round of 32 each sim (the pre-2026-06-11
#'   behaviour). Falls back to `"random"` with a warning if `groups` does
#'   not contain exactly groups A-L of 4 teams each.
#' @param verbose Logical. Print progress. Default TRUE.
#'
#' @return A list with `summary` (per-team round probabilities),
#'   `group_table` (group-position probabilities), `n_sims`, `elo_k`.
#' @export
simulate_world_cup <- function(predictions, groups, knockout,
                                n_sims = 10000L,
                                elo_k = 40,
                                bracket = c("fifa2026", "random"),
                                verbose = TRUE) {

  bracket <- match.arg(bracket)

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

  if (bracket == "fifa2026" &&
      !(n_groups == 12L && identical(group_letters, LETTERS[1:12]) &&
        all(table(groups$group) == 4L))) {
    warning("groups do not form the 12 groups A-L of 4 teams required for ",
            "the FIFA 2026 bracket; falling back to bracket = \"random\"",
            call. = FALSE)
    bracket <- "random"
  }

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
  ## preserves within-group sequence (needed for run-hot momentum) and so
  ## each group's matches occupy a contiguous block (needed for head-to-head).
  group_team_glob <- vector("list", n_groups)
  n_gm <- n_groups * choose(4L, 2L)            # 12 * 6 = 72
  m_grp  <- integer(n_gm)
  m_t1g  <- integer(n_gm); m_t2g <- integer(n_gm)
  m_t1l  <- integer(n_gm); m_t2l <- integer(n_gm)   # local slot 1..4 in group
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
      m_t1l[mc] <- match(t1, teams_g); m_t2l[mc] <- match(t2, teams_g)
      m_p1[mc] <- p1; m_pd[mc] <- pd; m_p2[mc] <- p2
      m_lam1[mc] <- max(0.2, l1); m_lam2[mc] <- max(0.2, l2)
    }
  }

  ## --- 3b. FIFA bracket setup (outside the hot loop) ---------------------
  if (bracket == "fifa2026") {
    spec <- wc2026_bracket_spec()
    n_ties <- length(spec$r32)
    ## Per tie side: type 1 = group winner, 2 = runner-up, 3 = third slot.
    ## idx = group index for types 1/2, slot number (1..8 in match order)
    ## for type 3.
    tie_type <- matrix(0L, n_ties, 2L)
    tie_idx  <- matrix(0L, n_ties, 2L)
    slot_cands <- list()
    for (t in seq_len(n_ties)) {
      for (s in 1:2) {
        side <- spec$r32[[t]][[s]]
        if (side[1] == "W") {
          tie_type[t, s] <- 1L
          tie_idx[t, s]  <- match(side[2], LETTERS)
        } else if (side[1] == "R") {
          tie_type[t, s] <- 2L
          tie_idx[t, s]  <- match(side[2], LETTERS)
        } else {
          slot_cands[[length(slot_cands) + 1L]] <-
            match(strsplit(side[2], "")[[1]], LETTERS)
          tie_type[t, s] <- 3L
          tie_idx[t, s]  <- length(slot_cands)
        }
      }
    }
    third_alloc <- build_third_allocation(slot_cands)
    round_maps <- list(spec$r16, spec$qf, spec$sf, spec$fin)
  }

  ## --- 4. Counters (integer-indexed, rows align with all_teams) ----------
  reach      <- matrix(0L, nrow = n_teams, ncol = 6)  # R32 R16 QF SF F Champ
  pos_counts <- matrix(0L, nrow = n_teams, ncol = 4)

  if (verbose) {
    cli::cli_alert_info(
      "Simulating {n_sims} World Cups (run-hot elo_k = {elo_k}, bracket = {bracket})...")
  }
  pb_every <- max(1L, n_sims %/% 20L)

  ## --- 5. Simulation loop ------------------------------------------------
  res1 <- integer(n_gm); res2 <- integer(n_gm)
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
      res1[mi] <- g1; res2[mi] <- g2

      eu <- elo_update_pair(elo_dyn[t1], elo_dyn[t2], g1, g2, elo_k)
      elo_dyn[t1] <- eu[1]; elo_dyn[t2] <- eu[2]
    }

    ## --- 5b. Rank groups (FIFA tiebreakers incl. head-to-head) -----------
    win_glob <- integer(n_groups); run_glob <- integer(n_groups)
    third_glob <- integer(n_groups); third_pts <- integer(n_groups)
    third_gd   <- integer(n_groups); third_gf  <- integer(n_groups)
    for (gi in seq_len(n_groups)) {
      gti <- group_team_glob[[gi]]
      p <- pts[gti]; d <- gf[gti] - ga[gti]; f <- gf[gti]
      tbk <- tb[((gi - 1L) * 4L + 1L):((gi - 1L) * 4L + 4L)]
      gm  <- ((gi - 1L) * 6L + 1L):(gi * 6L)
      ord <- rank_group_h2h(p, d, f, tbk,
                            m_t1l[gm], m_t2l[gm], res1[gm], res2[gm])
      ranked <- gti[ord]
      pos_counts[ranked[1], 1] <- pos_counts[ranked[1], 1] + 1L
      pos_counts[ranked[2], 2] <- pos_counts[ranked[2], 2] + 1L
      pos_counts[ranked[3], 3] <- pos_counts[ranked[3], 3] + 1L
      pos_counts[ranked[4], 4] <- pos_counts[ranked[4], 4] + 1L
      win_glob[gi] <- ranked[1]
      run_glob[gi] <- ranked[2]
      third_glob[gi] <- ranked[3]
      third_pts[gi]  <- p[ord[3]]
      third_gd[gi]   <- d[ord[3]]
      third_gf[gi]   <- f[ord[3]]
    }

    ## --- 5c. Best 8 third-placed teams -----------------------------------
    ## FIFA: points, GD, GF, then disciplinary points, then lots. The model
    ## has no card signal, so random stands in after GF.
    ord3 <- order(-third_pts, -third_gd, -third_gf, third_rand)
    qual3 <- sort(ord3[1:8])                     # group indices, ascending
    r32 <- c(win_glob, run_glob, third_glob[qual3])
    reach[r32, 1] <- reach[r32, 1] + 1L

    ## --- 5d. Knockouts ---------------------------------------------------
    if (bracket == "fifa2026") {
      ## Official bracket: resolve each R32 tie's sides, thirds via the
      ## precomputed eligibility allocation. Momentum carries via elo_dyn.
      slot_groups <- third_alloc[sum(2L^(qual3 - 1L)) + 1L, ]
      br <- integer(2L * n_ties)
      for (t in seq_len(n_ties)) {
        for (s in 1:2) {
          br[2L * (t - 1L) + s] <- switch(tie_type[t, s],
            win_glob[tie_idx[t, s]],
            run_glob[tie_idx[t, s]],
            third_glob[slot_groups[tie_idx[t, s]]])
        }
      }
      for (rd in 2:6) {
        kr <- play_knockout_round(br, WIN, DRAW, LAM,
                                  elo_dyn, elo_base, elo_k)
        w <- kr$winners
        elo_dyn <- kr$elo_dyn
        reach[w, rd] <- reach[w, rd] + 1L
        if (rd < 6L) br <- w[round_maps[[rd - 1L]]]
      }
    } else {
      ## Random reseeding (legacy behaviour).
      br <- sample(r32)
      for (rd in 2:6) {
        kr <- play_knockout_round(br, WIN, DRAW, LAM,
                                  elo_dyn, elo_base, elo_k)
        br <- kr$winners
        elo_dyn <- kr$elo_dyn
        reach[br, rd] <- reach[br, rd] + 1L
      }
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
       n_sims = n_sims, elo_k = elo_k, bracket = bracket)
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
