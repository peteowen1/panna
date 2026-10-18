# 03c_repair_asof_prior_sign.R
# One-off repair (panna#257, 2026-10-18): flip the defence of an expanding-window
# skill-SPM file fitted BEFORE the 2026-09-04 sign-convention switch, and tag it.
#
# Why: 03_skill_spm_asof.rds was fitted 2026-09-03, so its defense_spm is
# negative = good. fit_career_rapm() negates defense_spm assuming positive =
# good, so 09b shrank every snapshot's defence toward an inverted prior. Its
# defence ranked players almost exactly opposite to the live prior's (Spearman
# -0.81 on 6,082 players); negated, the two priors agree (0.907 overall).
#
# Negating is exact, not an approximation: the 09-03 fit used the old-convention
# RAPM target AND the old-convention sign constraints, so it is the same model
# with defence measured the other way round. The defence glmnet/xgb objects are
# DROPPED rather than kept: they still predict old-convention values, and no
# consumer reads them (09b and fit_career_rapm() read only the ratings tables).
# A full refit (03_skill_spm.R, asof_resume <- FALSE, ~1.5h) supersedes this.
#
# Refuses to run on a file that already carries a sign_convention tag, so it
# can never flip a file twice. Keeps the original as *.PRESIGN-BACKUP.rds.

suppressMessages(library(data.table))
devtools::load_all()

in_path  <- if (exists("asof_repair_path", inherits = FALSE)) asof_repair_path else
  file.path("data-raw", "cache-skills", "03_skill_spm_asof.rds")
live_path <- if (exists("asof_repair_live_path", inherits = FALSE)) asof_repair_live_path else
  file.path("data-raw", "cache-skills", "03_skill_spm.rds")
backup_path <- sub("\\.rds$", ".PRESIGN-BACKUP.rds", in_path)

x <- readRDS(in_path)
cat(sprintf("Read %s: %d cutoff years (%s)\n", in_path, length(x), paste(names(x), collapse = ",")))
tagged <- vapply(x, function(e) !is.null(e$sign_convention), logical(1))
if (any(tagged)) {
  stop(sprintf("%d of %d cutoff years already carry a sign_convention tag -- refusing to flip again.",
               sum(tagged), length(x)), call. = FALSE)
}

.def_corr <- function(entry, live) {
  a <- as.data.table(entry$defense_spm_ratings)[, .(player_id, a = defense_spm)]
  b <- as.data.table(live$defense_spm_ratings)[, .(player_id, b = defense_spm)]
  m <- merge(a, b, by = "player_id")
  list(n = nrow(m), rho = stats::cor(m$a, m$b, method = "spearman"))
}
live <- if (file.exists(live_path)) readRDS(live_path) else NULL
last <- names(x)[length(x)]
if (!is.null(live)) {
  before <- .def_corr(x[[last]], live)
  cat(sprintf("Before: cutoff %s defence vs live prior defence, Spearman %.3f (n = %d)\n",
              last, before$rho, before$n))
  if (before$rho > 0) stop("Defence already agrees with the live prior -- this file does not look pre-flip.", call. = FALSE)
}

if (!file.exists(backup_path)) {
  file.copy(in_path, backup_path)
  cat("Backed up original to", backup_path, "\n")
}

for (y in names(x)) {
  d <- x[[y]]$defense_spm_ratings
  for (col in intersect(c("def_glmnet", "def_xgb", "defense_spm"), names(d))) d[[col]] <- -d[[col]]
  x[[y]]$defense_spm_ratings <- d
  x[[y]]$defense_spm_glmnet <- NULL
  x[[y]]$defense_spm_xgb <- NULL
  x[[y]]$sign_convention <- CAREER_PANNA_SIGN_CONVENTION
  x[[y]]$sign_repair <- "defence negated 2026-10-18 from a pre-2026-09-04 fit; defence models dropped (panna#257)"
}

if (!is.null(live)) {
  after <- .def_corr(x[[last]], live)
  cat(sprintf("After:  cutoff %s defence vs live prior defence, Spearman %.3f (n = %d)\n",
              last, after$rho, after$n))
  if (after$rho < 0.5) stop("Flipped defence still disagrees with the live prior -- not saving.", call. = FALSE)
}
for (y in names(x)) .assert_skill_spm_sign_convention(x[[y]], paste("repaired cutoff year", y))

saveRDS(x, in_path)
cat(sprintf("SAVED %s (%.1f MB, %d cutoff years, all tagged %s)\n",
            in_path, file.info(in_path)$size / 1048576, length(x), CAREER_PANNA_SIGN_CONVENTION))
