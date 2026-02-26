# Create match filename for caching

With the hierarchical structure, filename is just the fbref_id. League
and season are encoded in the directory path.

## Usage

``` r
make_match_filename(fbref_id)
```

## Arguments

- fbref_id:

  FBref match ID (8-char hex)

## Value

Filename string (without path)
