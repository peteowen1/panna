# build_net_goals_page.R -- generate pkgdown/assets/net-goals.html from the
# live ledger.
#
# The readable half of the artifact. The other four scripts in this directory
# print to a console, which is fine for debugging and useless for checking the
# allocation by eye. This builds the page: a worked attacker and a worked
# defender traced action by action, the identity check, and the position and
# play-type tables.
#
# EVERYTHING IS COMPUTED LIVE. torp's equivalent page was built ad-hoc with no
# committed generator and drifted three rating vintages out of date before
# anyone noticed (torp/data-raw/04-analysis/build_net_points_scenarios.R says
# so in its own header). No cached CSV, no hand-entered figures.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/build_net_goals_page.R

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

LEAGUE   <- "ENG"
SEASON   <- "2024-2025"
OUT_HTML <- "pkgdown/assets/net-goals.html"
MIN_MINS <- 900

dir.create(dirname(OUT_HTML), recursive = TRUE, showWarnings = FALSE)

# ---- build -----------------------------------------------------------------
xg_model    <- readRDS("data-raw/cache/epv/xg_model.rds")
xpass_model <- readRDS("data-raw/cache/epv/xpass_model.rds")
epv_model   <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")

events  <- load_opta_match_events(LEAGUE, season = SEASON, source = "local")
lineups <- as.data.table(load_opta_lineups(LEAGUE, season = SEASON, source = "local"))
shot_lk <- panna:::.epv_shot_lookup(LEAGUE, SEASON)
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
  , .(match_id, home_team, away_team, home_team_id, away_team_id,
      home_score, away_score)]
adj  <- ng_build_adjacency(events, verbose = FALSE)
raw  <- ng_build_ledger(ep, adj = adj, fixtures = fx, verbose = FALSE)
pay  <- ng_spread_pools(raw, ep, lineups, verbose = FALSE)
sh   <- ng_shares()
ds   <- formals(ng_spread_pools)$dacts_share
dm   <- as.character(formals(ng_spread_pools)$dacts_measure)[2]

posmap <- as.data.table(get_player_positions(lineups, ep))
mins <- unique(lineups[, .(match_id, player_id, m = as.numeric(minutes_played))])[m > 0]
# ONE team per player-MATCH, never season-global: a season-global player->team
# lookup duplicates anyone who transfers mid-season, which is how A. Ramsdale
# appeared twice in an earlier cut of the leaderboard.
pteam <- unique(lineups[, .(match_id, player_id, team_name)])
names_lu <- unique(ep[!is.na(player_name), .(player_id, player_name)])

# ---- helpers ---------------------------------------------------------------
esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
num <- function(x, d = 3) formatC(round(x, d), format = "f", digits = d)
tbl <- function(dt, align = NULL, cls = "") {
  hd <- paste0("<th>", esc(names(dt)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(dt)), function(i) {
    cells <- vapply(seq_along(dt), function(j) {
      v <- dt[[j]][i]
      a <- if (is.numeric(v)) " class=\"n\"" else ""
      paste0("<td", a, ">", esc(v), "</td>")
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0("<table class=\"", cls, "\"><thead><tr>", hd, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table>")
}
sgn <- function(x, d = 3) {
  s <- formatC(round(x, d), format = "f", digits = d, flag = "+")
  cls <- ifelse(x > 0, "pos", ifelse(x < 0, "neg", "zero"))
  paste0("<span class=\"", cls, "\">", s, "</span>")
}

# ---- identity --------------------------------------------------------------
tt <- ng_check_team_totals(pay, fx, verbose = FALSE)
z  <- tt[, .(s = sum(own_total)), by = match_id]
ident <- data.table(
  quantity = c("team-matches", "cor(team total, that team's own goal difference)",
               "slope", "median |error|", "max |error|", "the two sides cancel to"),
  value = c(format(nrow(tt), big.mark = ","),
            num(cor(tt$own_total, tt$own_gd), 4),
            num(coef(lm(own_gd ~ own_total, tt))[2], 4),
            paste(num(median(abs(tt$err))), "goals"),
            paste(num(max(abs(tt$err))), "goals"),
            formatC(max(abs(z$s)), format = "e", digits = 2)))

big <- tt[order(-abs(own_gd))][1:6, .(`goal difference` = own_gd,
                                      `players sum to` = num(own_total))]

# ---- leaderboard -----------------------------------------------------------
season_tot <- function(entry = NULL) {
  d <- if (is.null(entry)) pay[!is.na(player_id)] else
    pay[!is.na(player_id) & entry %in% entry]
  d[, .(v = sum(value_own, na.rm = TRUE)), by = player_id]
}
lb <- merge(pay[!is.na(player_id), .(ng = sum(value_own, na.rm = TRUE)), by = player_id],
            mins[, .(mins = sum(m)), by = player_id], by = "player_id")
lb <- merge(lb, pay[!is.na(player_id) & entry == "offence",
                    .(off = sum(value_own, na.rm = TRUE)), by = player_id],
            by = "player_id", all.x = TRUE)
lb <- merge(lb, pay[!is.na(player_id) & entry == "defence",
                    .(def = sum(value_own, na.rm = TRUE)), by = player_id],
            by = "player_id", all.x = TRUE)
lb <- merge(lb, names_lu, by = "player_id", all.x = TRUE)
lb <- merge(lb, posmap, by = "player_id", all.x = TRUE)
lb <- merge(lb, unique(pteam[, .(player_id, team_name)])[!duplicated(player_id)],
            by = "player_id", all.x = TRUE)
lb <- lb[mins >= MIN_MINS][order(-ng)]
lb[, p90 := ng / mins * 90]

top_tbl <- lb[1:20, .(`#` = seq_len(20), player = player_name, team = team_name,
                      pos = position, mins = round(mins),
                      net = num(ng, 2), off = num(off, 2), def = num(def, 2),
                      `per 90` = num(p90))]
bot_tbl <- lb[order(ng)][1:10, .(player = player_name, team = team_name,
                                 pos = position, mins = round(mins),
                                 net = num(ng, 2), `per 90` = num(p90))]

# ---- worked examples -------------------------------------------------------
# One attacker and one defender, chosen as the top of each by season net goals
# so the page cannot be accused of picking a flattering row.
pick <- function(pos) lb[position %in% pos][1]
atk <- pick(c("Striker", "Attacking Midfielder"))
dfd <- pick("Defender")

walk_html <- function(pid, label) {
  # His single biggest match, then the run of actions around his biggest payment.
  pm <- pay[player_id == pid, .(v = sum(value_own, na.rm = TRUE)), by = match_id]
  mid <- pm[which.max(v)]$match_id
  f <- fx[match_id == mid]
  side <- function(tid) fifelse(tid == f$home_team_id, esc(f$home_team), esc(f$away_team))

  mine <- pay[match_id == mid & player_id == pid & !is.na(action_id)]
  if (!nrow(mine)) return("")
  aid <- mine[which.max(abs(value_own))]$action_id
  lo <- max(1L, aid - 4L); hi <- aid + 2L

  m <- ep[match_id == mid & action_id %between% c(lo, hi)]
  setorder(m, action_id)
  acts <- data.table(
    id = m$action_id,
    clock = sprintf("%02d:%02d", m$time_seconds %/% 60, round(m$time_seconds %% 60)),
    team = side(m$team_id),
    player = ifelse(is.na(m$player_name), "-", m$player_name),
    action = m$action_type, result = m$result,
    xPass = ifelse(is.na(m$xpass), "", num(m$xpass, 2)),
    EPV = num(m$epv), delta = num(m$epv_delta))

  # Read the UNSPREAD ledger here. ng_spread_pools() drops action_id when it
  # divides a pool across the eleven, so a walkthrough built from the spread
  # payments shows only the offence half and silently hides the defensive
  # entry -- on a page whose whole claim is that every action is booked twice.
  p <- raw[match_id == mid & action_id %between% c(lo, hi)]
  p <- merge(p, names_lu, by = "player_id", all.x = TRUE)
  setorder(p, action_id, -value_own)
  p <- p[abs(value_own) >= 5e-4]
  pays <- data.table(
    id = p$action_id, team = side(p$team_id),
    recipient = ifelse(is.na(p$player_name), "(team pool)", p$player_name),
    role = p$role, half = p$entry, paid = num(p$value_own, 4))

  tot <- pay[match_id == mid & player_id == pid, sum(value_own, na.rm = TRUE)]
  paste0(
    "<h3>", esc(label), " &mdash; ", esc(lb[player_id == pid]$player_name), "</h3>",
    "<p class=\"sub\">", esc(f$home_team), " ", esc(f$home_score), "&ndash;",
    esc(f$away_score), " ", esc(f$away_team), ". His net goals in this match: <b>",
    sgn(tot), "</b>.</p>",
    "<h4>What happened</h4>",
    "<p class=\"sub\"><code>delta</code> is the change in expected goals this action "
    , "caused, in his team's frame. It is the whole of what gets divided up.</p>",
    tbl(acts),
    "<h4>Who got paid</h4>",
    "<p class=\"sub\">Own frame: positive is good for that recipient. "
    , "<code>half</code> names which side of the double entry the payment sits on "
    , "&mdash; <code>offence</code> is the side that acted, <code>defence</code> the "
    , "side that conceded. Team pools are shown here <i>before</i> being divided "
    , "across the eleven on the pitch, so each action&rsquo;s two halves stay "
    , "visible on one line.</p>",
    tbl(pays))
}

# ---- tables ----------------------------------------------------------------
pg <- pay[!is.na(player_id), .(ng = sum(value_own, na.rm = TRUE)),
          by = .(match_id, player_id)]
pg <- merge(merge(pg, mins, by = c("match_id", "player_id")), posmap, by = "player_id")
pos_tbl <- pg[m >= 60, .(`player-games` = .N,
                         mean = num(mean(ng)), sd = num(sd(ng)),
                         p5 = num(quantile(ng, .05), 2),
                         p95 = num(quantile(ng, .95), 2),
                         `per 90` = num(sum(ng) / sum(m) * 90)),
              by = position][order(-as.numeric(mean))]

e <- merge(pay[!is.na(player_id) & !is.na(entry),
               .(v = sum(value_own, na.rm = TRUE)), by = .(match_id, player_id, entry)],
           pg[, .(match_id, player_id, position, m)], by = c("match_id", "player_id"))
pm2 <- pg[, .(tot_m = sum(m)), by = position]
e <- merge(e[, .(v = sum(v)), by = .(position, entry)], pm2, by = "position")
e[, p90 := v / tot_m * 90]
od <- dcast(e, position ~ entry, value.var = "p90", fill = 0)
od_tbl <- data.table(position = od$position, offence = num(od$offence),
                     defence = num(od$defence), total = num(od$offence + od$defence))
setorder(od_tbl, -total)

ptp <- pay[play_type != "pool"]
tot_abs <- sum(abs(ptp$value_own), na.rm = TRUE)
pt_tbl <- ptp[, .(n = format(.N, big.mark = ","),
                  total = num(sum(value_own, na.rm = TRUE), 1),
                  `share of abs` = paste0(num(100 * sum(abs(value_own), na.rm = TRUE) / tot_abs, 1), "%")),
              by = play_type]
setorder(pt_tbl, -n)
pt_tbl <- ptp[, .(n = .N, tv = sum(value_own, na.rm = TRUE),
                  ab = sum(abs(value_own), na.rm = TRUE)), by = play_type]
setorder(pt_tbl, -ab)
pt_tbl <- pt_tbl[, .(`play type` = play_type, n = format(n, big.mark = ","),
                     total = num(tv, 1),
                     `share of abs` = paste0(num(100 * ab / tot_abs, 1), "%"))]

pay[, kind := fifelse(play_type == "pool", "proxy (pool)", "named")]
k <- pay[!is.na(entry), .(ab = sum(abs(value_own), na.rm = TRUE)), by = .(entry, kind)]
k[, pct := 100 * ab / sum(ab)]
proxy_tbl <- k[order(entry, -ab), .(half = entry, kind,
                                    `share of all value` = paste0(num(pct, 1), "%"))]

# ---- page ------------------------------------------------------------------
css <- "
:root{--bg:#fbfbfa;--fg:#1c1b1a;--mut:#6b6864;--line:#e2e0dc;--pos:#1a7f5a;--neg:#b03a2e;--acc:#2a4d69}
@media (prefers-color-scheme:dark){:root:not([data-theme=light]){--bg:#17181a;--fg:#e8e6e3;--mut:#9a9691;--line:#2e3034;--pos:#4cc79a;--neg:#e8776a;--acc:#7aa7c7}}
:root[data-theme=dark]{--bg:#17181a;--fg:#e8e6e3;--mut:#9a9691;--line:#2e3034;--pos:#4cc79a;--neg:#e8776a;--acc:#7aa7c7}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.6 ui-sans-serif,system-ui,-apple-system,Segoe UI,Roboto,sans-serif}
.wrap{max-width:70rem;margin:0 auto;padding:2.5rem 16px 5rem}
h1{font-size:1.9rem;line-height:1.2;margin:0 0 .4rem;letter-spacing:-.02em}
h2{font-size:1.25rem;margin:2.6rem 0 .6rem;padding-top:1.4rem;border-top:1px solid var(--line);letter-spacing:-.01em}
h3{font-size:1.05rem;margin:1.8rem 0 .3rem}
h4{font-size:.85rem;text-transform:uppercase;letter-spacing:.08em;color:var(--mut);margin:1.2rem 0 .4rem}
p{margin:.5rem 0}
.sub{color:var(--mut);font-size:.9rem}
.lede{font-size:1.05rem;max-width:60ch}
code{font:13px/1.4 ui-monospace,SFMono-Regular,Menlo,monospace;background:color-mix(in srgb,var(--fg) 7%,transparent);padding:.1em .35em;border-radius:3px}
table{border-collapse:collapse;width:100%;margin:.6rem 0 1rem;font-size:.87rem;display:block;overflow-x:auto}
th,td{text-align:left;padding:.4rem .6rem;border-bottom:1px solid var(--line);white-space:nowrap}
th{font-weight:600;color:var(--mut);font-size:.78rem;text-transform:uppercase;letter-spacing:.05em}
td.n{text-align:right;font-variant-numeric:tabular-nums;font-family:ui-monospace,SFMono-Regular,Menlo,monospace}
tbody tr:hover{background:color-mix(in srgb,var(--acc) 7%,transparent)}
.pos{color:var(--pos);font-weight:600}.neg{color:var(--neg);font-weight:600}
.note{border-left:3px solid var(--acc);padding:.5rem 0 .5rem .9rem;margin:1.1rem 0;color:var(--mut);font-size:.9rem}
.meta{color:var(--mut);font-size:.82rem;margin-top:.3rem}
"

html <- paste0(
'<!doctype html><html lang="en"><head><meta charset="utf-8">',
'<meta name="viewport" content="width=device-width,initial-scale=1">',
'<title>Net Goals</title><style>', css, '</style></head><body><div class="wrap">',

'<h1>Net Goals</h1>',
'<p class="lede">Every action in a match changes the expected score. Net goals divides that change among the players who caused it, so that <b>each team&rsquo;s players sum to that team&rsquo;s own goal difference</b>. A 3&ndash;1 win gives the winners +2 and the losers &minus;2.</p>',
'<p class="meta">', esc(LEAGUE), ' ', esc(SEASON), ' &middot; ', format(nrow(ep), big.mark = ","),
' actions &middot; ', uniqueN(ep$match_id), ' matches &middot; generated ', format(Sys.Date()),
' &middot; <code>convention = "team"</code>, <code>exec_blame = ', sh$exec_blame,
'</code>, <code>named_share = ', sh$named_share, '</code>, <code>off_pool = ', sh$off_pool,
'</code>, <code>dacts_share = ', ds, '</code>, <code>dacts_measure = "', dm, '"</code></p>',

'<h2>Does it balance?</h2>',
'<p>Nothing is fitted and nothing is reconciled. The identity falls out of double entry: a team&rsquo;s total is the value of its own actions minus its opponent&rsquo;s, and that difference is already the goal difference.</p>',
tbl(ident),
'<p class="sub">The biggest wins of the season, and what the winning side&rsquo;s players actually sum to:</p>',
tbl(big),
'<div class="note">The error is not zero because panna&rsquo;s EPV asks &ldquo;who scores next <i>this half</i>&rdquo; and the shot override swaps model EPV for xG, so the telescoping terms nearly but not quite cancel. Forcing them to zero would need a reconciliation term, and a reconciliation can be larger than the thing it corrects &mdash; which is what torp measured when it tried.</div>',

'<h2>Worked examples</h2>',
'<p>Two players, each shown in his best match of the season, traced action by action. Neither was hand-picked: they are the top attacker and the top defender on the leaderboard below.</p>',
walk_html(atk$player_id, "An attacker"),
walk_html(dfd$player_id, "A defender"),

'<h2>Leaderboard</h2>',
'<p class="sub">Season net goals, minimum ', MIN_MINS, ' minutes. <code>off</code> and <code>def</code> are the two halves of the same number.</p>',
tbl(top_tbl),
'<h4>Bottom ten</h4>',
tbl(bot_tbl),

'<h2>By position</h2>',
'<p class="sub">One row per player-game, minimum 60 minutes. <code>sd</code> is the spread within the position &mdash; larger means the metric separates players inside it more.</p>',
tbl(pos_tbl),
'<h4>Offence and defence, per 90</h4>',
tbl(od_tbl),

'<h2>By play type</h2>',
'<p class="sub"><code>total</code> is the season sum in goals; <code>share of abs</code> is where the action is.</p>',
tbl(pt_tbl),
'<div class="note"><code>keeper_save</code> reads negative, and that is not a bug. The <i>stop</i> is paid on the shot row, inside <code>shot</code>; the <code>keeper_save</code> row carries only the rebound, and a rebound is on average bad for the side that conceded it. The offence/defence table above is the one that answers &ldquo;is the keeper paid&rdquo;.</div>',

'<h2>How much is actually observed?</h2>',
'<p>A named payment goes to the player the feed identified. A proxy payment is spread across whoever was on the pitch, because nobody was named. The defensive half is mostly proxy in any sport; this is what it costs here.</p>',
tbl(proxy_tbl),

'<p class="meta" style="margin-top:2.5rem">Regenerate with <code>Rscript data-raw/epv/net-goals/build_net_goals_page.R</code>. Every number on this page is computed live from the ledger &mdash; there is no cached copy to drift.</p>',
'</div></body></html>')

writeLines(html, OUT_HTML, useBytes = TRUE)
cli::cli_alert_success("wrote {OUT_HTML} ({round(file.size(OUT_HTML)/1024)} KB)")
