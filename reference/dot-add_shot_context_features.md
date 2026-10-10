# Add the pre-shot context a model needs to its shot features

Add the pre-shot context a model needs to its shot features

## Usage

``` r
.add_shot_context_features(
  features,
  shots,
  need,
  events = NULL,
  foot_history = NULL,
  what = "xG"
)
```

## Arguments

- features:

  Shot feature frame, one row per shot in `shots` (same order).

- shots:

  SPADL shot rows: `match_id`, `original_event_id`, `player_id`.

- need:

  Context columns the model reads (from its feature_cols).

- events:

  Full Opta events for these matches (see
  [`.shot_context()`](https://peteowen1.github.io/panna/reference/dot-shot_context.md)).
  Always required for context inputs: columns already on `shots` are not
  trusted, since other steps write same-named columns with other
  meanings.

- foot_history:

  [`.shot_foot_history()`](https://peteowen1.github.io/panna/reference/dot-shot_foot_history.md)
  output; needed for `foot_share`.

- what:

  Model name for messages.

## Value

`features` with the `need` columns added. Missing values stay NA: the
models were trained with NA meaning "no assist" / "too few earlier
shots", and read it that way (predict_xg honours `na_is_missing`).
