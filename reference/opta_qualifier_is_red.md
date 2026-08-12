# Does an Opta card event's qualifier JSON denote a dismissal?

Opta marks a straight red with qualifier 33 and a second yellow with 32,
both on a type_id 17 card event. This lived as a byte-identical private
copy in `splint_creation.R` and `wp_model.R`; the second copy even
carried a comment pointing at the first. Two copies means a new
dismissal qualifier gets added to one and not the other, and
minutes-played would then disagree with the win-probability model's
man-count about who was on the pitch – with nothing failing.

## Usage

``` r
opta_qualifier_is_red(qualifier_json)
```

## Arguments

- qualifier_json:

  A single qualifier-JSON string (or `NA`).

## Value

`TRUE` if the event is a dismissal. Unparseable or missing JSON returns
`FALSE`: a card we cannot read is not evidence of a dismissal, and the
alternative (erroring) would drop whole seasons over one malformed
event.
