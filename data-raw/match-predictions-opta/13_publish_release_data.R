# 13_publish_release_data.R
# Single gated publish of predictions-latest + blog-latest.
#
# ECOSYSTEM-FIX-PLAN.md PA5 (panna H-TORN): steps 09/10/10b/10c/10d/12 write
# LOCAL outputs only and register the files they want published this run into
# the global `publish_files` accumulator (declared in run_predictions_opta.R,
# initialized before step 9). This is the ONE place either tag actually gets
# uploaded, via vb_publish() -- hash first, upload with bounded retries,
# verify the live asset list, THEN write bus_manifest.json last. A failure in
# either tag aborts BEFORE that tag's manifest write, so consumers keep
# seeing the last consistent snapshot instead of a torn mix of predictions
# advanced / blog half-updated (the exact failure mode H-TORN documents: step
# 09 used to upload predictions-latest immediately, then 10/10b/10c/10d/12
# uploaded blog-latest piecewise, so a 10b failure -- the OOM-prone step --
# left predictions ahead of a half-updated blog release).
#
# Not meant to be run standalone: `publish_files` only exists when sourced
# from run_predictions_opta.R (guarded below regardless).

repo <- "peteowen1/pannadata"

if (!exists("publish_files", envir = .GlobalEnv)) {
  message("\n(standalone run -- no publish_files accumulator; nothing to publish. Run via run_predictions_opta.R.)")
} else {
  pf <- get("publish_files", envir = .GlobalEnv)

  pred_files <- unique(pf$predictions_latest)
  pred_files <- pred_files[file.exists(pred_files)]
  blog_files <- unique(pf$blog_latest)
  blog_files <- blog_files[file.exists(blog_files)]

  if (length(pred_files) > 0) {
    message(sprintf("\n=== Publishing predictions-latest (%d file(s)) ===", length(pred_files)))
    for (f in pred_files) message(sprintf("  %s", basename(f)))
    manifest <- vb_publish(pred_files, repo = repo, tag = "predictions-latest")
    message(sprintf("  predictions-latest published (generation %s)", manifest$generation))
  } else {
    message("\n  No predictions-latest files registered this run -- skipping publish")
  }

  if (length(blog_files) > 0) {
    message(sprintf("\n=== Publishing blog-latest (%d file(s)) ===", length(blog_files)))
    for (f in blog_files) message(sprintf("  %s", basename(f)))
    manifest <- vb_publish(blog_files, repo = repo, tag = "blog-latest")
    message(sprintf("  blog-latest published (generation %s)", manifest$generation))
  } else {
    message("\n  No blog-latest files registered this run -- skipping publish")
  }

  message("\n=== Release publish complete ===")
}
