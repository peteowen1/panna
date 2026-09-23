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
# The inputs (SPADL, EPV, xPass, xGOT) are cached to INPUTS; the ledger is
# rebuilt from live code on every run. Set NG_INPUTS_ONLY <- TRUE before
# sourcing to get ng_load_inputs() without building anything (for A/B scripts).
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

# ---- inputs (slow, cached) and ledger (fast, always from live code) ---------
# The slow part is SPADL + EPV + xPass + xGOT (minutes); the ledger itself takes
# seconds. So only the INPUTS are cached, and the ledger is rebuilt from the
# current R/epv_net_goals.R on every run -- a rule change never needs a cache
# bump, and A/B scripts can source the same inputs. Bump INPUT_VERSION when the
# inputs change; delete INPUTS to rebuild them from live code.
INPUTS <- sprintf("data-raw/cache/epv/net-goals/ng_inputs_%s_%s.rds", LEAGUE, SEASON)
INPUT_VERSION <- 1L
# The local model files are part of the key, so replacing one rebuilds the
# inputs without anyone remembering a bump. (The xGOT model comes from
# load_xgot_model() and the events from the local parquet: those still rely on
# INPUT_VERSION, or deleting INPUTS.)
MODEL_FILES <- c("data-raw/cache/epv/xg_model.rds", "data-raw/cache/epv/xpass_model.rds",
                 "data-raw/cache/epv/epv_model_xg_clean_full.rds")
ng_load_inputs <- function() {
  key <- list(version = INPUT_VERSION, models = unname(tools::md5sum(MODEL_FILES)))
  x <- if (file.exists(INPUTS)) readRDS(INPUTS) else NULL
  if (!is.null(x) && identical(x$input_key, key)) {
    say("reading cached inputs: ", INPUTS, " (built ", format(x$built), "; SPADL/EPV/xPass/xGOT are NOT rebuilt)")
    return(x)
  }
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
  # xGOT for the shot split (strike xG -> xGOT, finish xGOT -> outcome). Same
  # call as the xmetrics pipeline: shot events carry the goal-mouth point SPADL
  # drops, plus situation / blocked / body part so it matches training.
  # The published model (pannamodels), not the local cache: the cached
  # data-raw/cache/epv/xgot_model.rds is a pre-2026-09-03 build.
  xgot_model <- load_xgot_model()
  shot_ev <- as.data.frame(load_opta_shot_events(LEAGUE, season = SEASON, source = "local"))
  lk <- c("match_id", "event_id", "type_id", "goalmouth_y", "goalmouth_z",
          intersect(c("situation", "is_blocked", "body_part"), names(shot_ev)))
  ep  <- as.data.table(add_xgot_to_spadl(ep, xgot_model, shot_ev[, lk]))
  fx <- as.data.table(load_opta_fixtures(LEAGUE, season = SEASON, source = "local"))[
    , .(match_id, home_team, away_team, home_team_id, away_team_id, home_score, away_score)]
  adj <- ng_build_adjacency(events, verbose = FALSE)
  x <- list(input_key = key, built = Sys.time(), ep = ep, adj = adj, lineups = lineups, fx = fx)
  dir.create(dirname(INPUTS), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, INPUTS)
  say("built and cached the inputs in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
  x
}
if (!exists("NG_INPUTS_ONLY")) {
inp <- ng_load_inputs()
t0 <- Sys.time()
raw <- ng_build_ledger(inp$ep, adj = inp$adj, fixtures = inp$fx, lineups = inp$lineups, verbose = TRUE)
pay <- ng_spread_pools(raw, inp$ep, inp$lineups, verbose = FALSE)
keep_ep <- intersect(c("match_id", "action_id", "period_id", "time_seconds", "team_id",
                       "player_id", "player_name", "action_type", "result", "epv",
                       "epv_delta", "xpass", "xg", "xgot", "start_x", "start_y", "end_x", "end_y"),
                     names(inp$ep))
# CACHE keeps the finished ledger for the check scripts (ng_keeper_check.R and
# friends); it is rewritten on every run.
x <- list(ep = inp$ep[, ..keep_ep], lineups = inp$lineups, fx = inp$fx, raw = raw, pay = pay,
          positions = as.data.table(get_player_positions(inp$lineups, inp$ep)))
saveRDS(x, CACHE)
say("ledger built from live code in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
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
    # A shot's three steps, read apart: placement is finishing skill, the finish
    # is how keepers did against him (negative = good saves).
    role == "shot_strike",                                    "Shooting: placement (xG to xGOT)",
    role == "shot_finish",                                    "Shooting: against the keeper",
    role == "shot_aftermath",                                 "Shooting: what it left behind",
    role == "shooter",                                        "Shooting: no xGOT",
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
  "On the ball" = c("Passing", "Receiving a pass", "Carrying", "Take-ons",
                  "Shooting: placement (xG to xGOT)", "Shooting: against the keeper",
                  "Shooting: what it left behind", "Shooting: no xGOT", "Losing the ball"),
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

# The headline number is the LEDGER's row value (what the payments below sum
# to on the acting side), not the model's epv_delta, and "from" is where the
# ledger starts the row: a shot at its price V0 = xG + (1 - xG) * A (the shot
# aftermath), the row after a GOAL at 0, everything else at its own value. A row
# plus its change lands on the next row's start (checked below).
lv <- rp[entry %in% "offence", .(ledger = sum(value_own)), by = action_id]
af <- attr(raw, "shot_aftermath_fit")
stopifnot(!is.null(af))
price <- function(xg) xg + (1 - xg) * (af$intercept + af$slope * xg)
own_goal <- function(a) a$action_type == "shot" && a$result %in% "success" && a$epv_delta < 0
prev_goal <- c(FALSE, head(win$action_type == "shot" & win$result %in% "success", -1))

# Which step of a shot each payment belongs to. The ledger books the strike
# (xG -> xGOT), the finish (xGOT -> goal or save) and the aftermath (what the
# shot left behind) as separate payments with the same role, so label them by
# rebuilding the three amounts and matching each payment to one.
sh_ <- ng_shares()
step_of <- function(a, py) {
  if (a$action_type != "shot" || own_goal(a) || !is.finite(a$xgot)) return(rep(NA_character_, nrow(py)))
  xg <- a$epv; g <- as.numeric(a$result %in% "success")
  st <- c(strike = a$xgot - xg, finish = g - a$xgot)
  st["aftermath"] <- lv[action_id == a$action_id, ledger] - sum(st)
  cand <- rbindlist(lapply(names(st), function(k) {
    x <- st[[k]]
    keep <- sh_$shot_keep
    data.table(step = k, v = c(x * keep, x * (1 - keep), -x, -x * sh_$named_share, -x * (1 - sh_$named_share)))
  }))
  # The shooter's own rows carry their step in the role. Every other payment is
  # matched to the nearest rebuilt amount, and each amount is used once: when
  # two steps are the same size (a goal from xG 0.20 at xGOT 0.60 has strike
  # and finish both 0.40), nearest-match alone would label both "strike".
  out <- sub("^shot_", "", py$role)
  out[!out %in% names(st)] <- NA_character_
  used <- rep(FALSE, nrow(cand))
  for (j in which(is.na(out))) {
    d <- abs(cand$v - py$value_own[j]); d[used] <- Inf
    i <- which.min(d); used[i] <- TRUE; out[j] <- cand$step[i]
  }
  out
}
STEP_LAB <- c(strike = "strike: xG to xGOT", finish = "finish: xGOT to the result",
              aftermath = "what the shot left behind")
rows <- lapply(seq_len(nrow(win)), function(i) {
  a <- win[i]
  chg <- lv[action_id == a$action_id, ledger]
  chg <- if (length(chg)) chg else a$epv_delta
  before <- if (prev_goal[i]) 0 else if (a$action_type == "shot" && !own_goal(a)) price(a$epv) else a$epv
  py <- rp[action_id == a$action_id][order(-abs(value_own))]
  py[, step := step_of(a, py)]
  py <- py[order(entry, match(step, names(STEP_LAB)), -abs(value_own))]
  # Opta gives every action in its own team's attacking direction (left to
  # right, 0-100). Flip the away side's so one picture holds the whole passage:
  # the home team always attacks to the right.
  fl <- function(v) if (a$team_id == f$home_team_id) v else 100 - v
  pt <- function(px, py) if (is.finite(px) && is.finite(py)) list(x = round(fl(px), 1), y = round(fl(py), 1)) else NULL
  list(ord = i, is_goal = a$action_id == goal_id,
       from = pt(a$start_x, a$start_y), to = pt(a$end_x, a$end_y), clock = sprintf("%02d:%02d", a$time_seconds %/% 60, round(a$time_seconds %% 60)),
       team = team_lu[team_id == a$team_id]$team, player = a$player_name,
       action = a$action_type, result = a$result,
       value_before = round(before, 4), change = round(chg, 4),
       tags = list(xpass = if ("xpass" %in% names(a)) round(a$xpass, 2) else NULL),
       payments = lapply(seq_len(nrow(py)), function(j) list(
         player = if (is.na(py$player_id[j])) NA else py$player_name[j],
         team = team_lu[team_id == py$team_id[j]]$team,
         role = if (is.na(py$step[j])) py$role[j] else
           paste0(sub("^shot_.*", "shooter", py$role[j]), " · ", STEP_LAB[[py$step[j]]]),
         entry = if ("entry" %in% names(py)) py$entry[j] else NA,
         value = round(py$value_own[j], 4))))
})
# Double entry, checked on the passage the page shows: in each side's OWN frame
# the gaining side books +v and the conceding side -v, so an action's payments
# sum to zero. (value_home does NOT: both halves point the same way for home.)
# ... and each row's start plus its change is the next row's start, in the
# next row's frame (a possession change flips the sign). A break here means the
# page's "from" is wrong, not the ledger.
st_ <- vapply(rows, function(r) r$value_before, numeric(1)); ch_ <- vapply(rows, function(r) r$change, numeric(1))
same_ <- head(win$team_id, -1) == tail(win$team_id, -1)
land_ <- head(st_ + ch_, -1); nxt_ <- tail(st_, -1)
gap_ <- abs(ifelse(same_, land_ - nxt_, land_ + nxt_))
gap_[head(win$action_type == "shot" & win$result %in% "success", -1)] <- 0   # a goal ends at 1; the kick-off restarts
say("walkthrough chain: worst gap between a row's end and the next row's start ", signif(max(gap_), 3))
if (max(gap_) > 1e-3) stop("walkthrough 'from' values do not chain (", max(gap_), ")")
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
}  # end if (!exists("NG_INPUTS_ONLY"))
