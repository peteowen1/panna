# Log OS-level memory (RSS) alongside R's own heap at a checkpoint

The panna#128 and \#133 OOM hunts both hinged on the same observation:
[`gc()`](https://rdrr.io/r/base/gc.html) only tracks the R-managed heap,
while the failures showed up only in OS-level resident set size (VmRSS)
— and both hunts reinvented this checkpoint inline. Drop
`.log_rss("after step 3")` at suspected boundaries in a pipeline script,
deploy, and read the log; the RSS-vs-heap gap localizes hidden-copy
growth that heap numbers cannot see.

## Usage

``` r
.log_rss(label = "", verbose = TRUE)
```

## Arguments

- label:

  Checkpoint label included in the log line.

- verbose:

  Whether to print (same contract as `progress_msg`).

## Value

The RSS in MB (invisibly), `NA` if unavailable.

## Details

RSS sources, in order: `/proc/self/status` (Linux — the GHA runner, the
platform that matters), `ps -o rss=` (macOS/other unix), else `NA`
(Windows has no cheap equivalent without adding a dependency; the R-heap
number still prints).
