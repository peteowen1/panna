# 08c_sign_audit.R
#
# Wave 2 sign audit (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 5.3, veto class):
# fits each candidate on the FULL panel (the shipping-shape fit, via
# run_candidate()) and audits glmnet coefficients at lambda.min against the
# clearly-signed sets (.panel_offense_sign_constraints() /
# .panel_defense_sign_constraints()).
#
# What the audit actually tests, per candidate class:
#   - UNCONSTRAINED candidates (S1, S2): any wrong-signed coefficient among
#     the clearly-signed set is a live confound -- sec 5.3 says the fix is
#     finding the confound (or adopting the constraint), never flipping the
#     display.
#   - CONSTRAINED candidates (S3+): violations are impossible by
#     construction (glmnet box constraints), so the audit instead reports
#     which constrained features sit AT the bound (coef == 0) while their
#     UNCONSTRAINED sibling fit (S2, same pooling) is wrong-signed -- i.e.
#     features where the constraint is actively masking a confound rather
#     than merely formalizing an already-correct sign. That list is the
#     investigation queue, not an automatic veto.
#   - Role-deviation coefficients: top |coef| deviations reported for the
#     sec 5.3 role-coherence eyeball ("individually explainable or shrunk
#     to zero").
#
# Outputs (data-raw/spm-redesign/):
#   wave2_sign_audit.csv     one row per (candidate, target, audited column)
#   wave2_deviation_top.csv  top deviation coefficients per candidate
#
# Config overrides (exists() pattern, set before sourcing):
#   audit_candidate_ids   default c("S2", "S3")  (S2 = the unconstrained
#                         confound probe at shipping pooling; S3 = the
#                         constrained default candidate. Add S4a/S4b/S5 if
#                         the gate table crowns one of them.)
#   audit_seed            default 1 (match the bake-off's fold seed)
#
# Run from panna/ (relative cache paths):
#   Rscript data-raw/spm-redesign/08c_sign_audit.R

audit_candidate_ids <- if (exists("audit_candidate_ids", inherits = FALSE)) audit_candidate_ids else c("S2", "S3")
audit_seed <- if (exists("audit_seed", inherits = FALSE)) audit_seed else 1

source(file.path("data-raw", "spm-redesign", "05c_candidates.R"))
suppressMessages(library(data.table))

panel_bundle <- readRDS(file.path("data-raw", "cache-opta", "spm_panel.rds"))
panel <- panel_bundle$panel
attr(panel, "target_provenance") <- panel_bundle$target_provenance

# Expected-direction lookup from the canonical constraint lists.
# Offense list: lower = 0 -> expected sign >= 0.
# Defense list (internal convention, negative = good): upper = 0 on GOOD
# defensive features -> expected <= 0; lower = 0 on BAD features -> >= 0.
.expected_signs <- function(target) {
  sc <- if (target == "offense") .panel_offense_sign_constraints() else .panel_defense_sign_constraints()
  rbind(
    if (!is.null(sc$lower)) data.table(base_feature = names(sc$lower), expected = "nonneg"),
    if (!is.null(sc$upper)) data.table(base_feature = names(sc$upper), expected = "nonpos")
  )
}

.audit_fit <- function(cv_fit, candidate, target) {
  cf <- as.matrix(stats::coef(cv_fit, s = "lambda.min"))
  cf_dt <- data.table(column = rownames(cf), coef = cf[, 1])
  cf_dt <- cf_dt[column != "(Intercept)"]
  cf_dt[, base_feature := .panel_base_feature_of(column)]
  cf_dt[, is_deviation := column %in% cv_fit$panna_metadata$dev_names]
  cf_dt[, `:=`(candidate = candidate, target = target,
               constrained = isTRUE(cv_fit$panna_metadata$sign_constraints))]
  cf_dt
}

all_coefs <- list()
for (cid in audit_candidate_ids) {
  cfg <- candidate_configs[[cid]]
  if (is.null(cfg)) cli::cli_abort("Unknown candidate id {.val {cid}}.")
  cli::cli_h1(sprintf("Sign audit: fitting %s on the full panel", cid))
  fits <- run_candidate(panel, cfg, seed = audit_seed)
  all_coefs[[paste0(cid, "_off")]] <- .audit_fit(fits$offense, cid, "offense")
  all_coefs[[paste0(cid, "_def")]] <- .audit_fit(fits$defense, cid, "defense")
}
coefs <- rbindlist(all_coefs)

expected <- rbind(.expected_signs("offense")[, target := "offense"],
                  .expected_signs("defense")[, target := "defense"])

audit <- merge(coefs, expected, by = c("target", "base_feature"))
tol <- 1e-10
audit[, violation := (expected == "nonneg" & coef < -tol) | (expected == "nonpos" & coef > tol)]
audit[, at_zero := abs(coef) <= tol]

fwrite(audit[order(candidate, target, -abs(coef))],
       file.path("data-raw", "spm-redesign", "wave2_sign_audit.csv"))

cli::cli_h1("Violations (wrong-signed among the clearly-signed set)")
viol <- audit[violation == TRUE]
if (nrow(viol) == 0) cli::cli_alert_success("None.") else print(viol[order(candidate, -abs(coef)),
  .(candidate, target, column, coef, expected)])

# Constraint-masking report: constrained candidate has the feature pinned at
# zero while an unconstrained candidate in this audit run is wrong-signed on
# the SAME (target, column).
uncon_wrong <- audit[constrained == FALSE & violation == TRUE, .(target, column, uncon_candidate = candidate, uncon_coef = coef)]
con_zero <- audit[constrained == TRUE & at_zero == TRUE, .(target, column, candidate)]
masking <- merge(con_zero, uncon_wrong, by = c("target", "column"))
cli::cli_h1("Constraint-masking (constrained-to-zero features that go wrong-signed unconstrained)")
if (nrow(masking) == 0) cli::cli_alert_success("None.") else print(masking[order(candidate, target)])

# Role-deviation top coefficients (sec 5.3 role coherence) ----
dev_top <- coefs[is_deviation == TRUE & abs(coef) > 0][order(candidate, target, -abs(coef)),
  head(.SD, 20), by = .(candidate, target)]
fwrite(dev_top, file.path("data-raw", "spm-redesign", "wave2_deviation_top.csv"))
cli::cli_h1("Top role-deviation coefficients (per candidate/target)")
print(dev_top[, head(.SD, 8), by = .(candidate, target)])

cli::cli_alert_success("Sign-audit artifacts written to data-raw/spm-redesign/")
