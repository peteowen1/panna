# Read a pre-match skill snapshot, transparently handling streamed chunks

`.estimate_prematch_skills_batch(stream_dir = )` returns a file path
(character length-1) in place of each data.table element. Callers that
need to work with either mode (streaming or the old in-memory list)
should route every read through this helper rather than assuming the
element is already a data.table.

## Usage

``` r
.read_skill_chunk(x)
```

## Arguments

- x:

  One element of
  [`.estimate_prematch_skills_batch()`](https://peteowen1.github.io/panna/reference/dot-estimate_prematch_skills_batch.md)'s
  returned list: either a data.table/data.frame (in-memory mode) or a
  length-1 character file path (streaming mode). `NULL` passes through
  unchanged.

## Value

The data.table for that date, or `NULL`.
