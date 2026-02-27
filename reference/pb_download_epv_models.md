# Download EPV Models from GitHub Releases

Downloads pre-trained EPV models from GitHub releases.

## Usage

``` r
pb_download_epv_models(
  repo = "peteowen1/pannadata",
  tag = "epv-models",
  dest = NULL
)
```

## Arguments

- repo:

  GitHub repository (default: peteowen1/pannadata)

- tag:

  Release tag (default: epv-models)

- dest:

  Destination directory. If NULL, uses pannadata/data/opta/models/

## Value

Invisibly returns path to models
