library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
L <- readRDS(file.path(out, "wpa_ledger_liv_d0.rds")); w <- copy(L$w)
H <- readRDS(file.path(out, "wp_home_frame.rds"))
fh <- c("time_remaining", "m_home", "red_home", "is_second_half", "is_extra_time")
w[, `:=`(m_home = score_diff, red_home = red_card_diff)]
Pposs <- function(d, add) { x <- copy(d); x[, m_home := m_home + add * fifelse(is_home == 1, 1, -1)]
  h <- predict(H, as.matrix(x[, ..fh])); fifelse(d$is_home == 1, h, 1 - h) }
p_m <- Pposs(w, 0); p_up <- Pposs(w, 1); p_dn <- Pposs(w, -1); e <- w$epv
w[, wp_s := ifelse(e >= 0, (1 - e) * p_m + e * p_up, (1 + e) * p_m - e * p_dn)]
setorder(w, match_id, period_id, time_seconds, action_id)
w[, `:=`(wn = shift(wp_s, type = "lead"), tn2 = shift(team_id, type = "lead")), by = match_id]
w[is.na(wn), `:=`(wn = wp_label, tn2 = team_id)]
w[, wpa_s := fifelse(tn2 == team_id, wn - wp_s, (1 - wn) - wp_s)]
liv <- "c8h9bw1l82s06h77xxrelzhur"; m <- "4mr2fpdoxrh2o9m6dj4lpopw4"
x <- w[match_id == m]; x[, i := .I]
x[, side := fifelse(team_id == liv, "LIV", "OPP")]
x[, liv_wp_pub := fifelse(side == "LIV", wp, 1 - wp)][, liv_wp_str := fifelse(side == "LIV", wp_s, 1 - wp_s)]
g <- x[is_goal == 1 | (shift(score_diff, type="lead") != score_diff), i][1]
cat("first goal row", g, "\n")
show <- function(rows, title) { cat("\n==", title, "==\n")
  print(x[rows, .(min = round(time_seconds/60 + 45*(period_id-1), 1), side, player = substr(player_name,1,16), action_type, result,
                  score = score_diff, epv = round(epv,3), liv_wp_pub = round(liv_wp_pub,3), wpa_pub = round(wpa,4),
                  liv_wp_str = round(liv_wp_str,3), wpa_str = round(wpa_s,4))]) }
show((g-7):(g+2), "build-up to Liverpool's first goal (wpa in the ACTING team's frame)")
z <- x[score_diff == 0 & time_seconds < 900 & side != shift(side, type="lead"), i]
show((z[3]-3):(z[3]+2), "early 0-0 stretch with a turnover")
s <- x[, .(pub = mean(abs(wpa)), str = mean(abs(wpa_s))), by = .(turn = side != shift(side, type="lead"))]
print(s)
