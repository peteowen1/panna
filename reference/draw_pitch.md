# Draw a Football Pitch

Creates a ggplot2 object with a football pitch overlay using the SPADL
coordinate system (0-100 for both x and y axes).

## Usage

``` r
draw_pitch(background = c("white", "green"))
```

## Arguments

- background:

  Character. Background style: "white" (default) or "green".

## Value

A ggplot2 object with pitch markings.

## Examples

``` r
if (FALSE) { # \dontrun{
draw_pitch()
draw_pitch("green")
} # }
```
