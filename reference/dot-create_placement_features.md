# Create goal-mouth placement features

Turns the raw goal-mouth crossing point (goalmouth_y, goalmouth_z) into
the feature(s) the xGOT model learns from. This is the heart of the
model: how you encode "where in the frame" decides what the tree can
discover.

## Usage

``` r
.create_placement_features(gm_y, gm_z)
```

## Arguments

- gm_y, gm_z:

  Numeric vectors of goal-mouth y (horizontal) and z (height)
  coordinates. May contain NA (off-target / missing).

## Value

data.frame of placement features, one row per shot. Column names become
model features - keep them descriptive (e.g. dist_to_near_post).

## Details

The empirical signal from EPL 2024-25 on-target shots (illustrative):
distance to nearest post: hug-post(\<1)=0.358 near=0.248 mid=0.106
central=0.069 height band: low(\<5)=0.386 mid(5-12)=0.284
high(12-20)=0.056 top(\>20)=0.287 Note the height effect is U-SHAPED
(mid-height = keeper's easy reach = worst; both low-and-tucked and
top-corner convert well). XGBoost handles non-linearity, so RAW
gm_y/gm_z already let it find the corners - but engineered features
(distance-to-near-post, height) sharpen it on limited data and make
feature-importance readable.

Geometry constants available: GOAL_POST_Y_LEFT (45.2), GOAL_POST_Y_RIGHT
(54.8), GOAL_POST_Y_MID (50), GOAL_CROSSBAR_Z (38).
