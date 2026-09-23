# build_net_goals_artifacts.R -- data for the two net goals pages.
#
# Writes two JSON files, in the shared shape torp's pages use
# (vault/plans/NET-LEDGER-PARITY.md, "The contract both sides meet"):
#
#   data-raw/cache/epv/net-goals/ng_categories_artifact.json  every player's net goals per
#       game, split by play type, plus team pool share and anchor to the real
#       goal difference. The parts add up to net_goals EXACTLY (gated below).
#   data-raw/cache/epv/net-goals/ng_walkthrough.json          one passage, action by action,
#       up to and including a goal, with every payment each action made.
#
# The ledger build is the slow part, so it is cached to CACHE and reused. Delete
# the file to rebuild from live code.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/build_net_goals_artifacts.R

suppressPackageStartupMessages({library(data.table); library(jsonlite)})
devtools::load_all(quiet = TRUE)
say <- function(...) cat(..., "\n", sep = "")

LEAGUE   <- "ENG"
SEASON   <- "2024-2025"
MIN_GMS  <- 10
WALK_HOME <- "Liverpool FC"; WALK_AWAY <- "Manchester City FC"  # fixtures carry the "FC"
WALK_N   <- 30                                             # actions up to the goal
CACHE    <- sprintf("data-raw/cache/epv/net-goals/ng_ledger_%s_%s.rds", LEAGUE, SEASON)
OUT_DIR  <- "data-raw/cache/epv/net-goals"   # gitignored

# ---- build (or read the cached build) ---------------------------------------
# Bump CACHE_VERSION whenever the cached contents change (v2 added the action
# coordinates the walkthrough's pitch plot needs), so an old cache is rebuilt
# rather than read with columns missing.
CACHE_VERSION <- 2L
x <- if (file.exists(CACHE)) readRDS(CACHE) else NULL
if (!is.null(x) && identical(x$cache_version, CACHE_VERSION)) {
  say("reading cached ledger: ", CACHE, " (delete it to rebuild from live code)")
} else {
  t0 <- Sys.time()
  events  <- load_opta_match_events(LEAGUE, season = SEASON, source = "local")
  lineups <- as.data.table(load_opta_lineups(LEAGUE, season = SEASON, source = "local"))
  shot_lk <- panna:::.epv_shot_lookup(LEAGUE, SEASON)
  xg_model    <- readRDS("data-raw/cache/epv/xg_model.rds")
  xpass_model <- readRDS("data-raw/cache/epv/xpass_model.rds")
  epv_model   <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")
  spadl <- convert_opta_to_spadl(events)
  ch  <- create_possession_chains(spadl)
  lab <- label_actions_with_outcomes(ch, add_next_chain_outcome(classify_chain_outcomes(ch)))
  lab <- create_next_goal_labels(lab)
  if (epv_model$method == "xg") lab <- create_next_xg_labels(lab)
  ep  <- calculate_action_epv(lab, create_epv_features(lab, n_prev = 3), epv_model,
                              xg_model = xg_model, league = LEAGUE, season = SEASON,
                              shot_lookup = shot_lk)
  ep  <- as.data.table(add_xpass_to_spadl(ep, xpass_model))
  fx <- as.data.table(load_opta_fixtures(LEAGUE, season = SEASON, source = "local"))[
    , .(match_id, home_team, away_team, home_team_id, away_team_id, home_score, away_score)]
  adj <- ng_build_adjacency(events, verbose = FALSE)
  raw <- ng_build_ledger(ep, adj = adj, fixtures = fx, verbose = FALSE)
  pay <- ng_spread_pools(raw, ep, lineups, verbose = FALSE)
  keep_ep <- intersect(c("match_id", "action_id", "period_id", "time_seconds", "team_id",
                         "player_id", "player_name", "action_type", "result", "epv",
                         "epv_delta", "xpass", "xg", "start_x", "start_y", "end_x", "end_y"),
                       names(ep))
  x <- list(cache_version = CACHE_VERSION, ep = ep[, ..keep_ep], lineups = lineups, fx = fx, raw = raw, pay = pay,
            positions = as.data.table(get_player_positions(lineups, ep)))
  dir.create(dirname(CACHE), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, CACHE)
  say("built and cached the ledger in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
}
ep <- x$ep; lineups <- x$lineups; fx <- x$fx; raw <- as.data.table(x$raw)
pay <- as.data.table(x$pay)
say("actions ", nrow(ep), " | payments ", nrow(pay), " | matches ", uniqueN(pay$match_id))
stopifnot(nrow(pay) > 0, uniqueN(pay$match_id) >= 300)

names_lu <- unique(ep[!is.na(player_name), .(player_id, player_name)], by = "player_id")
team_lu  <- unique(rbind(fx[, .(team_id = home_team_id, team = home_team)],
                         fx[, .(team_id = away_team_id, team = away_team)]), by = "team_id")

# =============================================================================
# 1. PLAY-TYPE BREAKDOWN
# =============================================================================
# The same three parts as torp: named payments (split by play type), the
# player's slice of the team pools, and the anchor to the real goal difference.
p <- pay[!is.na(player_id) & nzchar(as.character(player_id))]
p[, is_pool := grepl("^pool_", role)]
say("\nWhere the named value sits (role x play type, |goals|):")
print(dcast(p[is_pool == FALSE, .(v = round(sum(abs(value_own)), 1)), by = .(play_type, role)],
            play_type ~ role, value.var = "v", fill = 0))

# The play-type label: one column per thing a player DID. Role decides first,
# because the same play type pays different people -- on a shot, `shooter` is
# the man who hit it and `defender` the keeper or blocker who stopped it; on a
# pass, `defender` is the player named for cutting it out. Pool rows are not
# play types: they are the team's share. First cut 2026-09-23, to be walked
# through with Pete on real players before it is treated as settled.
lab <- function(play_type, role) {
  data.table::fcase(
    grepl("^pool_", role),                                    "Team pool share",
    role == "shooter",                                        "Shooting",
    role == "receiver",                                       "Receiving a pass",
    role == "stopper_rebound",                                "Keeper: rebound after a save",
    role == "defender" & play_type == "shot",                 "Stopping shots",
    role == "defender" & play_type == "pass",                 "Cutting out passes",
    role == "defender",                                       "Defending other actions",
    play_type %in% c("pass", "cross"),                        "Passing",
    play_type == "take_on",                                   "Take-ons",
    play_type == "dribble",                                   "Carrying",
    play_type == "aerial",                                    "Aerial duels",
    play_type %in% c("tackle", "interception"),               "Tackles & interceptions",
    play_type == "ball_recovery",                             "Ball recoveries",
    play_type == "clearance",                                 "Clearances",
    play_type %in% c("ball_touch", "dispossessed"),           "Losing the ball",
    play_type == "foul",                                      "Fouls",
    grepl("^keeper_", play_type),                             "Keeper: handling",
    default = "Other")
}
p[, cat := lab(play_type, role)]

pg <- ng_player_game(pay, lineups, verbose = FALSE)
pg <- ng_reconcile_margin(pg, fx, verbose = TRUE)
pg <- as.data.table(pg)

cats <- p[, .(v = sum(value_own)), by = .(match_id, player_id, cat)]
cats <- rbind(cats, pg[, .(match_id, player_id, cat = "Anchor to the real goal difference", v = ng_recon)])

# THE GATE: the parts rebuild each player-match's net_goals exactly.
.chk <- merge(cats[, .(tot = sum(v)), by = .(match_id, player_id)],
              pg[, .(match_id, player_id, net_goals)], by = c("match_id", "player_id"))
stopifnot(nrow(.chk) == nrow(pg))
.gap <- max(abs(.chk$tot - .chk$net_goals))
say("\n=== do the categories sum to net goals? ===  worst |gap| ", signif(.gap, 3),
    " over ", nrow(.chk), " player-matches")
if (!is.finite(.gap) || .gap > 1e-9) stop("categories do not add up to net_goals (worst gap ", .gap, "); nothing written")

gms <- pg[, .(gms = uniqueN(match_id), mins = round(mean(minutes_played, na.rm = TRUE))), by = player_id]
gms <- gms[gms >= MIN_GMS]
per <- merge(cats[player_id %in% gms$player_id, .(v = sum(v)), by = .(player_id, cat)], gms, by = "player_id")
per[, v := round(v / gms, 3)]
w <- dcast(per, player_id + gms + mins ~ cat, value.var = "v", fill = 0)
netv <- pg[player_id %in% gms$player_id, .(net = round(sum(net_goals) / uniqueN(match_id), 3)), by = player_id]

# one team and position per player: the modal one by games
tmf <- pg[, .N, by = .(player_id, team_id)][order(player_id, -N, team_id)][, .SD[1], by = player_id]
tmf <- merge(tmf[, .(player_id, team_id)], team_lu, by = "team_id", all.x = TRUE)[, team_id := NULL]
pos <- x$positions
pos_col <- intersect(c("position_group", "position", "pos"), names(pos))[1]
if (!is.na(pos_col)) {
  pos <- unique(pos[, .(player_id, pos = get(pos_col))], by = "player_id")
} else pos <- data.table(player_id = character(), pos = character())
n0 <- nrow(w)
w <- merge(w, names_lu[, .(player_id, name = player_name)], by = "player_id", all.x = TRUE)
w <- merge(w, tmf, by = "player_id", all.x = TRUE)
w <- merge(w, pos, by = "player_id", all.x = TRUE)
w <- merge(w, netv, by = "player_id")
stopifnot(nrow(w) == n0, !anyDuplicated(w$player_id))
setorder(w, -net)

CATS <- setdiff(names(w), c("player_id", "gms", "mins", "name", "team", "pos", "net"))
fam <- list(
  "On the ball" = c("Passing", "Receiving a pass", "Carrying", "Take-ons", "Shooting", "Losing the ball"),
  "Winning it back" = c("Tackles & interceptions", "Ball recoveries", "Aerial duels", "Clearances",
                        "Cutting out passes", "Defending other actions", "Fouls"),
  "Goalkeeping" = c("Stopping shots", "Keeper: handling", "Keeper: rebound after a save"),
  "Other named" = "Other",
  "Team pool & anchor" = c("Team pool share", "Anchor to the real goal difference"))
fam <- lapply(fam, function(m) intersect(m, CATS))
stopifnot(setequal(unlist(fam), CATS))

write_json(list(sport = "football", league = LEAGUE, season = SEASON, unit = "goals / game",
                n = nrow(w), min_games = MIN_GMS,
                generated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                methods = CATS,
                families = lapply(names(fam), function(n) list(name = n, members = fam[[n]])),
                players = w),
           file.path(OUT_DIR, "ng_categories_artifact.json"), auto_unbox = TRUE, na = "null", digits = 6)
say("wrote ", file.path(OUT_DIR, "ng_categories_artifact.json"), " (", nrow(w), " players, ", length(CATS), " categories)")

# =============================================================================
# 2. WALKTHROUGH: WALK_N actions up to and including a goal
# =============================================================================
f <- fx[home_team == WALK_HOME & away_team == WALK_AWAY]
stopifnot(nrow(f) == 1)
m <- ep[match_id == f$match_id][order(action_id)]
g <- m[action_type %like% "^shot" & result == "success"]
stopifnot(nrow(g) > 0)
goal_id <- g$action_id[1]
win <- m[action_id <= goal_id][(.N - WALK_N + 1):.N]
rp <- raw[match_id == f$match_id & action_id %in% win$action_id]
rp <- merge(rp, names_lu, by = "player_id", all.x = TRUE)
rp[, side := fifelse(value_own >= 0, "gain", "concede")]

rows <- lapply(seq_len(nrow(win)), function(i) {
  a <- win[i]
  py <- rp[action_id == a$action_id][order(-abs(value_own))]
  # Opta gives every action in its own team's attacking direction (left to
  # right, 0-100). Flip the away side's so one picture holds the whole passage:
  # the home team always attacks to the right.
  fl <- function(v) if (a$team_id == f$home_team_id) v else 100 - v
  pt <- function(px, py) if (is.finite(px) && is.finite(py)) list(x = round(fl(px), 1), y = round(fl(py), 1)) else NULL
  list(ord = i, is_goal = a$action_id == goal_id,
       from = pt(a$start_x, a$start_y), to = pt(a$end_x, a$end_y), clock = sprintf("%02d:%02d", a$time_seconds %/% 60, round(a$time_seconds %% 60)),
       team = team_lu[team_id == a$team_id]$team, player = a$player_name,
       action = a$action_type, result = a$result,
       value_before = round(a$epv, 4), change = round(a$epv_delta, 4),
       tags = list(xpass = if ("xpass" %in% names(a)) round(a$xpass, 2) else NULL),
       payments = lapply(seq_len(nrow(py)), function(j) list(
         player = if (is.na(py$player_id[j])) NA else py$player_name[j],
         team = team_lu[team_id == py$team_id[j]]$team,
         role = py$role[j], entry = if ("entry" %in% names(py)) py$entry[j] else NA,
         value = round(py$value_own[j], 4))))
})
# Double entry, checked on the passage the page shows: in each side's OWN frame
# the gaining side books +v and the conceding side -v, so an action's payments
# sum to zero. (value_home does NOT: both halves point the same way for home.)
rv <- rp[, .(net = sum(value_own)), by = action_id]
.de <- max(abs(rv$net))
say("walkthrough: ", nrow(win), " actions, ", nrow(rp), " payments; worst double-entry gap ", signif(.de, 3))
if (!is.finite(.de) || .de > 1e-9) stop("walkthrough payments break double entry (", .de, "); nothing written")

tot <- pg[match_id == f$match_id, .(player_id, team_id, net = round(net_goals, 3),
                                    anchor = round(ng_recon, 3))]
tot <- merge(tot, names_lu, by = "player_id", all.x = TRUE)
tot <- merge(tot, team_lu, by = "team_id")[order(-net)]
write_json(list(sport = "football", unit = "goals",
                pitch = list(kind = "football", x = c(0, 100), y = c(0, 100), attack = "home team attacks to the right"),
                match = list(id = f$match_id, home = f$home_team, away = f$away_team,
                             home_score = as.numeric(f$home_score), away_score = as.numeric(f$away_score)),
                goal_action = goal_id, rows = rows,
                headline = paste0(g$player_name[1], "'s goal"),
                source = paste0("Built by <code>panna/data-raw/epv/net-goals/build_net_goals_artifacts.R</code> from ",
                                "<code>ng_build_ledger()</code> (payments per action, before team pools are shared out) and ",
                                "<code>ng_reconcile_margin()</code> (match totals). ", LEAGUE, " ", SEASON, "."),
                totals = tot[, .(player = player_name, team, net, anchor)]),
           file.path(OUT_DIR, "ng_walkthrough.json"), auto_unbox = TRUE, na = "null", digits = 6)
say("wrote ", file.path(OUT_DIR, "ng_walkthrough.json"))
