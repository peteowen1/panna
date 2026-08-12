# Classify an error from a release/network operation

The cardinal versebus rule: when classification is uncertain, the answer
is "transient" (abort), never "absent" (overwrite). Only a positively
confirmed 404 on the tag/asset listing may classify as "absent".

## Usage

``` r
vb_classify_error(e)
```

## Arguments

- e:

  a condition object

## Value

one of "absent", "transient"
