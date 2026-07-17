# Opta Event Reference

Reference for Opta F24 event data type IDs and qualifier IDs used in the
panna EPV pipeline.

**Every entry below is sourced.** Prior versions of this doc asserted
qualifier IDs with no evidence trail, several of which turned out wrong
(see “Corrections from the prior version” at the bottom). The rule going
forward: nothing goes in this table without either (a) a citation to the
production code that consumes it, or (b) an empirical cross-reference
against real ground-truth box scores, with the method and sample size
stated. Anything not yet verified this way is listed in its own
“Unconfirmed” section — don’t promote it without doing the work.

## Event Type IDs (`type_id`)

**Source of truth: `R/spadl_conversion.R`’s `OPTA_TYPE_NAMES`**
(corpus-validated against the real feed, with explicit
`# was "X" (WRONG)` correction comments for every code this doc
previously had wrong). Reproduced here for convenience — if the two ever
disagree, trust the R file, not this copy.

| type_id | Event                     | type_id | Event             | type_id                | Event                        |
|---------|---------------------------|---------|-------------------|------------------------|------------------------------|
| 1       | Pass                      | 28      | End Delay         | 55                     | Offside Provoked             |
| 2       | Offside Pass              | 30      | End               | 56                     | Shield Ball Opp              |
| 3       | Take On                   | 32      | Start             | 57                     | Foul Throw-in                |
| 4       | Foul                      | 34      | Team Set Up       | 58                     | Penalty Faced                |
| 5       | Ball Out                  | 35      | Position Change   | 59                     | Keeper Sweeper               |
| 6       | Corner Awarded            | 36      | Jersey Change     | 60                     | Chance Missed                |
| 7       | Tackle                    | 37      | Collection End    | 61                     | Ball Touch                   |
| 8       | Interception              | 38      | Temp Goal         | 63                     | Temp Save                    |
| 9       | Turnover                  | 39      | Temp Attempt      | 64                     | Resume                       |
| 10      | Save                      | 40      | Formation Change  | 65                     | Contentious Referee Decision |
| 11      | Claim                     | 41      | Punch             | 66                     | Possession Data              |
| 12      | Clearance                 | 42      | Good Skill        | 67                     | 50/50                        |
| 13      | Miss                      | 43      | Deleted Event     | 68                     | Referee Drop Ball            |
| 14      | Post                      | 44      | Aerial            | 69                     | Failed To Block              |
| 15      | Attempt Saved             | 45      | Challenge         | 70                     | Injury Time Announcement     |
| 16      | Goal                      | 47      | Rescinded Card    | 71                     | Coach Setup                  |
| 17      | Card                      | 49      | Ball Recovery     | 72                     | Caught Offside               |
| 18      | Player Off                | 50      | Dispossessed      | 73                     | Other Ball Contact           |
| 19      | Player On                 | 51      | Error             | 74                     | Blocked Pass                 |
| 20      | Player Retired            | 52      | Keeper Pick-up    | 75, 76, 79, 80, 81, 84 | Unknown (see note)           |
| 21      | Player Returns            | 53      | Cross Not Claimed | 83                     | Att One on One               |
| 22      | Player Becomes Goalkeeper | 54      | Smother           |                        |                              |
| 23      | Goalkeeper Becomes Player |         |                   |                        |                              |
| 24      | Condition Change          |         |                   |                        |                              |
| 25      | Official Change           |         |                   |                        |                              |
| 27      | Start Delay               |         |                   |                        |                              |

**Notably corrected from the prior version of this doc** (these were
flat-out wrong): 50 is **Dispossessed**, not “Blocked Pass” (that’s 74).
51 is **Error**, not “Delay of Play”. 67 is **50/50**, not “Offside”
(that’s 72, “Caught Offside”). 74 is **Blocked Pass**, not “Injury
Clearance”. This exact set of corrections independently cost
`inthegame-blog` a live-stats bug (#414/#419, 2026-07-16) before this
doc got fixed — the blog was carrying its own copy of the same wrong
labels.

`75/76/79/80/81/84`: genuinely not in any public F24 reference; named
“Unknown” deliberately rather than guessed. See `spadl_conversion.R`’s
header comment for per-code corpus frequency if you’re trying to pin one
down.

## Qualifier IDs (`qualifierId`)

The JSON format on each event row: `{"qualifierId": value, ...}` (keys
are qualifier IDs as strings, values are the qualifier’s payload or
null).

### Confirmed — cited to production code

| qualifier_id      | Meaning                                                                                                                     | Source                                                                                                                                                        |
|-------------------|-----------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 2                 | Cross                                                                                                                       | `spadl_conversion.R`’s `parse_opta_qualifiers`; also the basis of `inthegame-blog`’s `is_cross` flag (worker/src/index.js, 2026-07-16)                        |
| 4                 | Through Ball                                                                                                                | `parse_opta_qualifiers`                                                                                                                                       |
| 9                 | Penalty                                                                                                                     | `parse_opta_qualifiers`                                                                                                                                       |
| 28                | Own Goal                                                                                                                    | `parse_opta_qualifiers`; own-goal EPV delta = `-1 - prev_epv`                                                                                                 |
| 82                | Blocked shot                                                                                                                | `spadl_conversion.R:111` comment — type-15 “Attempt Saved” includes defender-blocked shots, flagged by this qualifier; on-target = (type 15 ∧ ¬q82) ∨ type 16 |
| 214               | Big Chance                                                                                                                  | `parse_opta_qualifiers`                                                                                                                                       |
| 15                | Head (body part)                                                                                                            | `parse_opta_qualifiers`; independently re-confirmed 2026-07-16 (below)                                                                                        |
| 140 / 141         | End X / End Y                                                                                                               | Used throughout `spadl_conversion.R` and `inthegame-blog`’s worker for pass/shot end coordinates                                                              |
| 23                | Fast break (situation)                                                                                                      | `worker/src/index.js`’s shot-situation qualifier handling                                                                                                     |
| 22 / 24 / 25 / 26 | Shot situation family (penalty / setpiece / corner / freekick — exact assignment per `worker/src/index.js`)                 | same                                                                                                                                                          |
| 31                | Yellow card (base flag — present on every card: plain yellow, second yellow, straight red alike; not a discriminator alone) | Empirical 2026-07-16: 100% across 398 checked card events                                                                                                     |
| 32                | Second yellow (this dismissal was two yellows, not a straight red)                                                          | Empirical 2026-07-16: 100% of 41 `secondYellow`-positive players, 0% of 265 plain-yellow                                                                      |
| 33                | Red card — straight reds only                                                                                               | Empirical 2026-07-16: only 55.4% of 92 `redCard`-positive players (the rest are second-yellow reds, flagged by q32 instead — see below)                       |

### Confirmed — empirically, 2026-07-16

Method: for each target stat, took every WC 2002–2026 player-match where
panna’s own `opta_player_stats.parquet` box score shows the stat \> 0
(“positive”), pulled that player’s raw events for the match from
`events_World_Cup.parquet` (which carries full `qualifier_json`), and
checked which qualifier IDs appear on the relevant event type for
positive players but not for a matched negative sample. Full population,
not a subsample, unless noted.

| qualifier_id | Meaning                                    | Evidence                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|--------------|--------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **14**       | **Last man** (last-ditch defensive action) | On type-7 (Tackle) events: 100% of 60 sampled `lastManTackle`-positive players, 0% of 74 negative — perfect split. On type-12 (Clearance) events: 72% of `clearanceOffLine`-positive players (n=124), 0% of 2814 plain-block negatives — the same qualifier flags both stats depending on event type, and clearance-off-line events independently average x≈4.9 (i.e. right on the own goal line, consistent with “last-ditch”). This ID was in **neither** the old version of this doc nor anything checked into `inthegame-blog` — new. **Qualifiers are event-type-scoped — this has nothing to do with cards.** `wp_model.R`/`splint_creation.R`’s `detect_red_in_qj` separately checks q14 on type_id 17 (Card) events assuming it also flags red cards; empirically it never appears on any card event (0% across 398 checked). That’s an unrelated, apparently-dead/wrong check — filed as **panna#141**, not fixed here. |
| **94**       | **Outfielder block**                       | Restricted to type 10/74/69/12 events: 100% of 2281 `outfielderBlock`-positive players carry it, 0.08% (≈6/7638) of negatives — essentially a clean discriminator.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| **168**      | **Flick on**                               | On type 1/44 events: 100% of 60 sampled `totalFlickOn`-positive players, 0% of 104 negative.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| **75**       | **Won corners** (moderate confidence)      | On type-6 (Corner Awarded) events: 57% of 3383 `wonCorners`-positive players vs 4.6% of 2805 negatives — a real ~12x lift, but not a clean 100/0 split like the three above. Likely correct qualifier but attribution (who exactly gets the row) may need refinement before treating this as fully nailed down; don’t ship a derivation off this without a tighter re-check.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |

### Unconfirmed — do not guess, do the work first

- **Six-yard block** (`sixYardBlock`): no qualifier found that
  discriminates it from a plain block, even after restricting to
  confirmed q94 block events and checking event coordinates
  (six-yard-block events average x≈13.6, statistically indistinguishable
  from plain-block events at x≈13.8 — clearance-off-line events, by
  contrast, are cleanly separated at x≈4.9). Whatever marks this stat,
  it isn’t in the qualifier set checked here. Needs either a wider
  qualifier search or a different signal entirely (a paired event? a
  distinct sub-type of q94?).
- **Left foot / right foot split**: q15=Head is solid (re-confirmed
  above), but the foot-side IDs are contested across sources — the prior
  version of this doc claimed 73=left foot (marked “implied”, i.e. never
  actually checked); other code in this ecosystem has used both 36 and
  72 for foot-side at different points. Don’t trust any of these without
  a dedicated empirical check the same way the four qualifiers above
  were confirmed.
- **Zone/coordinate qualifiers 55/56/102/103/230/231**: the prior
  version of this doc listed these as start/end coordinate qualifiers,
  but they conflict with 140/141 (confirmed above, and the qualifiers
  actually consumed by production code for end coordinates). Not
  re-verified this session — treat the prior claims here as unverified,
  not wrong-but-unconfirmed.

### Outcome Field

| outcome | Meaning                                                     |
|---------|-------------------------------------------------------------|
| 1       | Success (pass completed, tackle won, shot on target, etc.)  |
| 0       | Failure (pass incomplete, tackle lost, shot blocked/missed) |

For shots (type_id 13–16), the type_id itself indicates outcome: 16 =
Goal, 13/14/15 = not a goal (15 additionally splits into on-target vs
blocked via qualifier 82 — see above).

## SPADL Mapping

[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
maps Opta events to SPADL actions — see `R/spadl_conversion.R` directly
for the current mapping rather than duplicating it here (a stale copy in
this doc is exactly the failure mode this rewrite is trying to stop).

## Duel Events (type_id 44, 7)

Opta records both participants in a duel as separate consecutive events
(Row N: Player A, outcome indicates win/loss; Row N+1: Player B, the
opponent).
[`merge_duel_rows()`](https://peteowen1.github.io/panna/reference/merge_duel_rows.md)
combines these into single rows, keeping the winner’s row and adding
`opponent_player_id`/ `opponent_player_name` from the loser’s row.

## Coordinate System

Opta uses a 0–100 scale for both X and Y: - **X**: 0 = own goal line,
100 = opponent goal line - **Y**: 0 = right touchline (attacking
perspective), 100 = left touchline - **Center**: (50, 50); **goal
center**: (100, 50); **penalty spot**: ≈(88, 50) - **6-yard box**: X \>
94, Y between ≈37–63; **18-yard box**: X \> 83, Y between ≈21–79

[`normalize_spadl_coordinates()`](https://peteowen1.github.io/panna/reference/normalize_spadl_coordinates.md)
ensures teams always attack toward X = 100.

## Corrections from the prior version of this doc (2026-07-16)

- Type-ID table replaced wholesale — it was carrying the pre-correction
  labels for codes 50, 51, 53, 54, 55, 56, 57, 67, 68, 70, 74 (see
  `spadl_conversion.R`’s own `(WRONG)` comments).
- Qualifier table restructured into confirmed (with source)
  vs. unconfirmed, instead of one flat table with no way to tell a
  verified entry from a guess.
- Qualifiers 76 (“Six Yard Block”) and 77 (“Saved Off Line”) from the
  prior version could not be reproduced against real ground truth (see
  “Unconfirmed” above) — dropped rather than carried forward unverified.
- Four new qualifiers empirically confirmed that weren’t in the prior
  version at all: 14 (last man), 94 (outfielder block), 168 (flick on),
  75 (won corners, moderate confidence).

## References

- Opta F24 Feed Documentation (proprietary)
- SPADL Paper: <https://arxiv.org/abs/1802.07127>
- socceraction library: <https://github.com/ML-KULeuven/socceraction>
- Empirical qualifier confirmation, 2026-07-16:
  `events_World_Cup.parquet` (2002–2026, incl. `qualifier_json`)
  cross-referenced against `opta_player_stats.parquet` ground truth
