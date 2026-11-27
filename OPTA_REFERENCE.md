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

| type_id | Event | type_id | Event | type_id | Event |
|----|----|----|----|----|----|
| 1 | Pass | 28 | End Delay | 55 | Offside Provoked |
| 2 | Offside Pass | 30 | End | 56 | Shield Ball Opp |
| 3 | Take On | 32 | Start | 57 | Foul Throw-in |
| 4 | Foul | 34 | Team Set Up | 58 | Penalty Faced |
| 5 | Ball Out | 35 | Position Change | 59 | Keeper Sweeper |
| 6 | Corner Awarded | 36 | Jersey Change | 60 | Chance Missed |
| 7 | Tackle | 37 | Collection End | 61 | Ball Touch |
| 8 | Interception | 38 | Temp Goal | 63 | Temp Save |
| 9 | Turnover | 39 | Temp Attempt | 64 | Resume |
| 10 | Save | 40 | Formation Change | 65 | Contentious Referee Decision |
| 11 | Claim | 41 | Punch | 66 | Possession Data |
| 12 | Clearance | 42 | Good Skill | 67 | 50/50 |
| 13 | Miss | 43 | Deleted Event | 68 | Referee Drop Ball |
| 14 | Post | 44 | Aerial | 69 | Failed To Block |
| 15 | Attempt Saved | 45 | Challenge | 70 | Injury Time Announcement |
| 16 | Goal | 47 | Rescinded Card | 71 | Coach Setup |
| 17 | Card | 49 | Ball Recovery | 72 | Caught Offside |
| 18 | Player Off | 50 | Dispossessed | 73 | Other Ball Contact |
| 19 | Player On | 51 | Error | 74 | Blocked Pass |
| 20 | Player Retired | 52 | Keeper Pick-up | 75, 76, 79, 80, 81 | Unknown (see note) |
| 21 | Player Returns | 53 | Cross Not Claimed | 83 | Attempted (missed) tackle; dropped in SPADL, see note |
| 22 | Player Becomes Goalkeeper | 54 | Smother | 84 | Deleted After Review (community label) |
| 23 | Goalkeeper Becomes Player |  |  |  |  |
| 24 | Condition Change |  |  |  |  |
| 25 | Official Change |  |  |  |  |
| 27 | Start Delay |  |  |  |  |

**Notably corrected from the prior version of this doc** (these were
flat-out wrong): 50 is **Dispossessed**, not “Blocked Pass” (that’s 74).
51 is **Error**, not “Delay of Play”. 67 is **50/50**, not “Offside”
(that’s 72, “Caught Offside”). 74 is **Blocked Pass**, not “Injury
Clearance”. This exact set of corrections independently cost
`inthegame-blog` a live-stats bug (#414/#419, 2026-07-16) before this
doc got fixed — the blog was carrying its own copy of the same wrong
labels.

`75/76/79/80/81`: genuinely not in any public F24 reference; named
“Unknown” deliberately rather than guessed. See `spadl_conversion.R`’s
header comment for per-code corpus frequency if you’re trying to pin one
down.

**84 = Deleted After Review** (confirmed on real rows 2026-11-27; label
from the community-maintained `data/opta-truth.json` in
github.com/withqwerty/football-docs, said to be checked by hand against
the Stats Perform F24 appendices: community-checked, not
vendor-confirmed). On EPL 2023-24 to 2025-26
(`events_consolidated/events_EPL.parquet`, 283 type-84 rows), **every**
row carries q436 (`preReviewEventType` in opta-truth.json), and its
value is the type the event had before the review: 16 (Goal) on 139, 4
(Foul) 41, 2 (Offside Pass) 29, 61 (Ball Touch) 26, 17 (Card) 23. So
these are mostly VAR-cancelled goals. They sit at (0, 0) and carry the
cancelled shot’s goalmouth (102/103) and keeper-position (230/231)
qualifiers. Compare type 43 (Deleted Event): all 42,902 rows carry q144
(`deletedEventType`), commonly 83, 61 and 1.

**83 is a defender’s missed tackle** (decided 2026-11-27,
[panna#532](https://github.com/peteowen1/panna/issues/532)). Older code
called it “Att One on One” and SPADL mapped it to `take_on`;
opta-truth.json says **“Attempted tackle”**, and the EPL rows agree with
that. On 2024-25 and 2025-26 (19,112 type-83 rows): median x 39.6 (the
actor’s own half; type-3 take-ons sit at 66.0), outcome 1 on only 10.7%,
and it sits **inside the other team’s possession**: the event just
before is an opponent pass on 10,493 and the event just after an
opponent pass on 9,625. When a type-3 take-on shares its second, 98.2%
are by the other team with mirrored x (sum 99.9). That is a defender who
went for the ball and missed, not an attacker’s one-on-one. Since panna
0.3.134 SPADL drops every 83 row:
[`attach_missed_tackles()`](https://peteowen1.github.io/panna/reference/attach_missed_tackles.md)
(`spadl_conversion.R`) makes the defender the duel loser
(`opponent_player_id`, `opponent_from_83`) of the attacking team’s next
action within 3 s, and
[`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md)
moves half of that action’s positive credit to him as blame. Its outcome
is ignored: outcome 1 is not a won ball (the defender’s team has the
next event on 16.5% of outcome-1 rows vs 15.6% of outcome-0 rows, EPL
2025-26). The box-score stat `attOneOnOne` (`spm_opta.R`) is a separate
Opta stat and is not affected.

## Qualifier IDs (`qualifierId`)

The JSON format on each event row: `{"qualifierId": value, ...}` (keys
are qualifier IDs as strings, values are the qualifier’s payload or
null).

### Confirmed — cited to production code

| qualifier_id | Meaning | Source |
|----|----|----|
| 2 | Cross | `spadl_conversion.R`’s `parse_opta_qualifiers`; also the basis of `inthegame-blog`’s `is_cross` flag (worker/src/index.js, 2026-07-16) |
| 4 | Through Ball | `parse_opta_qualifiers` |
| 9 | Penalty | `parse_opta_qualifiers` |
| 28 | Own Goal | `parse_opta_qualifiers`; own-goal EPV delta = `-1 - prev_epv` |
| 82 | Blocked shot | `spadl_conversion.R:111` comment — type-15 “Attempt Saved” includes defender-blocked shots, flagged by this qualifier; on-target = (type 15 ∧ ¬q82) ∨ type 16 |
| 214 | Big Chance | `parse_opta_qualifiers` |
| 15 | Head (body part) | `parse_opta_qualifiers`; independently re-confirmed 2026-07-16 (below) |
| 140 / 141 | End X / End Y | Used throughout `spadl_conversion.R` and `inthegame-blog`’s worker for pass/shot end coordinates |
| 23 | Fast break (situation) | `worker/src/index.js`’s shot-situation qualifier handling |
| 22 / 24 / 25 / 26 | Shot situation family. opta-truth.json (community-checked): 22 regular play, 24 set piece, 25 from corner, 26 free kick. Penalty is q9, not 22 | `worker/src/index.js`; `opta_scraper.py` situation block |
| 31 | Yellow card (base flag — present on every card: plain yellow, second yellow, straight red alike; not a discriminator alone) | Empirical 2026-07-16: 100% across 398 checked card events |
| 32 | Second yellow (this dismissal was two yellows, not a straight red) | Empirical 2026-07-16: 100% of 41 `secondYellow`-positive players, 0% of 265 plain-yellow |
| 33 | Red card — straight reds only | Empirical 2026-07-16: only 55.4% of 92 `redCard`-positive players (the rest are second-yellow reds, flagged by q32 instead — see below) |

### Confirmed — empirically, 2026-07-16

Method: for each target stat, took every WC 2002–2026 player-match where
panna’s own `opta_player_stats.parquet` box score shows the stat \> 0
(“positive”), pulled that player’s raw events for the match from
`events_World_Cup.parquet` (which carries full `qualifier_json`), and
checked which qualifier IDs appear on the relevant event type for
positive players but not for a matched negative sample. Full population,
not a subsample, unless noted.

| qualifier_id | Meaning | Evidence |
|----|----|----|
| **14** | **Last man** (last-ditch defensive action) | On type-7 (Tackle) events: 100% of 60 sampled `lastManTackle`-positive players, 0% of 74 negative — perfect split. On type-12 (Clearance) events: 72% of `clearanceOffLine`-positive players (n=124), 0% of 2814 plain-block negatives — the same qualifier flags both stats depending on event type, and clearance-off-line events independently average x≈4.9 (i.e. right on the own goal line, consistent with “last-ditch”). This ID was in **neither** the old version of this doc nor anything checked into `inthegame-blog` — new. **Qualifiers are event-type-scoped — this has nothing to do with cards.** `wp_model.R`/`splint_creation.R`’s `detect_red_in_qj` separately checks q14 on type_id 17 (Card) events assuming it also flags red cards; empirically it never appears on any card event (0% across 398 checked). That’s an unrelated, apparently-dead/wrong check — filed as **panna#141**, not fixed here. |
| **94** | **Outfielder block** | Restricted to type 10/74/69/12 events: 100% of 2281 `outfielderBlock`-positive players carry it, 0.08% (≈6/7638) of negatives — essentially a clean discriminator. |
| **168** | **Flick on** | On type 1/44 events: 100% of 60 sampled `totalFlickOn`-positive players, 0% of 104 negative. |
| **75** | **Won corners** (moderate confidence) | On type-6 (Corner Awarded) events: 57% of 3383 `wonCorners`-positive players vs 4.6% of 2805 negatives — a real ~12x lift, but not a clean 100/0 split like the three above. Likely correct qualifier but attribution (who exactly gets the row) may need refinement before treating this as fully nailed down; don’t ship a derivation off this without a tighter re-check. |

### Confirmed — empirically, 2026-07-23 (panna#176)

Method: EPL 2021-22 → 2025-26 `opta_shot_events.parquet` joined to
`events_EPL.parquet` on `(match_id, event_id)`, `type_id==15` (“Attempt
Saved”) shots cross-referenced against raw `qualifier_json`.
Investigation prompted by the xGOT blocked-shot contamination fix
(panna#176) and a follow-up question: can a blocked shot’s real location
be recovered at all, given its own `goalmouth_y`/`goalmouth_z`
(q102/103) is a meaningless placeholder (see `backfill_goalmouth.py`)?

- **q82 (“Blocked shot”) has been flat and stable since at least
  2011-12** — ~53-58% of every EPL season’s `type_id==15` shots carry
  it, no trend, no step change. What DID change, starting mid-2019-20
  and complete by 2020-21, is that Opta’s feed switched from omitting
  q102/103 entirely for a blocked shot (true NA — pre-2020 goalmouth
  coverage of ~45-48% tracks almost exactly the non-blocked share) to
  always filling a value, including a meaningless placeholder
  (`goalmouth_z==19`, the frame-height midpoint) for shots that
  structurally have nothing to report. Full season-by-season table in
  `backfill_goalmouth.py`’s docstring.
- **A blocked shot’s REAL location IS recoverable — from a companion
  event, not the shot itself.** Every blocked shot is immediately
  followed (within the next 1-3 events, in raw event order) by a
  separate `type_id==10` (“Save”) event, credited to the DEFENDING team,
  carrying **q94** (“Outfielder block”, confirmed above) — this is Opta
  crediting the specific defender who made the block, distinct from the
  shooter’s own “Attempt Saved” row. That companion event’s `x`/`y` are
  REAL coordinates (not a placeholder). Across every EPL 2024-25 blocked
  shot: the companion `Save`+q94 event is findable 75.7% of the time
  (n=2,201/2,907; the rest are scrambles/rebounds where a second shot or
  corner intervenes before the Save event). Its `x` has genuine spread —
  median 11.4, IQR 7.6-15.4, range 0.1-40.6 — NOT a degenerate constant.
  13.0% sit at `x<=6` (effectively a last-ditch, goal-line/“basically a
  save” block); 63% fall in the 5-20 range (a real defensive
  intervention inside/around the box); the remaining 26% extend to
  x=20-40 (blocks further out, in the run of play).
- **This refines, but does not resolve, the “Six-yard block” item
  below.** That earlier investigation checked `type_id==12` (Clearance)
  events matched to the `sixYardBlock` STAT name specifically and found
  no location signal there. This finding is a DIFFERENT, more directly
  useful population — the `type_id==10`+q94 event Opta logs as the
  direct counterpart to a blocked SHOT — and it does carry a real
  signal. Not (yet) wired into any panna code; xGOT still correctly
  excludes ALL blocked shots regardless of block distance (the shot
  itself never reached the goal frame, so xGOT has nothing to score
  there either way) — this is documented here as a recoverable signal
  for anyone who later wants a “how save-like was this block” feature
  elsewhere (e.g. EPV credit, a defensive-blocking rating).

### Confirmed — empirically, 2026-11-27 (foot side)

- **20 = right foot, 72 = left foot** (opta-truth.json `rightFoot` /
  `leftFoot`). On 19,357 EPL 2024-25 + 2025-26 shots joined to
  `opta_shot_events.parquet` on `(match_id, event_id)`: every
  `RightFoot` shot carries q20 (9,519) or q21 (88), every `LeftFoot` q72
  (6,246), every `Head` q15 (3,504); no shot carries two. This settles
  the old “36 / 72 / 73” doubt: 72 is left, 73 is not used.
- **21 = other body part.** The scraper
  (`pannadata/scripts/opta/opta_scraper.py` body-part block, and
  `heal_shot_events.py`) has no branch for it, so those 88 shots (0.45%)
  are stored as `RightFoot`. Left as is: an `Other` level would be new
  to the xG model.

### Confirmed — empirically, 2026-11-26 (panna#175; corrects the 2026-07-23 entry)

- **230 / 231** \| **Goalkeeper x / y position** (football-docs
  `opta-truth.json`: `gkX` / `gkY`; community-checked, not
  vendor-confirmed). NOT the shot’s goal-line crossing point. On 147,120
  shot events (types 13-16, local
  `events_consolidated/events_EPL.parquet`): q230 sits at a goal line
  (\<= 12 or \>= 88) on 98.8-99.8% of every shot type; q231 is median
  50.0, sd 2.5-3.4 for every type, whatever happened to the shot; on
  **Goals**, where q102 is certainly the crossing point, q231 correlates
  -0.05 with it (identical on 0.5% of 14,744). Where a keeper stands
  fits all of that; a shot coordinate fits none of it.
- **102** \| **Goal-mouth y for every shot type, misses included.**
  Misses spread wide on q102 (sd 13.65 on EPL) because misses go wide;
  that was never a scaling error.
- History: 2026-07-23 to 2026-11-26 the scraper and
  `backfill_goalmouth.py` read Miss-shot `goalmouth_y` from q231, on a
  “Post shots land on the posts” sample of a few values and on q231
  being “tightly inside the frame” for misses, which is the symptom, not
  evidence. Result: 74.6% of 1,375,032 unblocked misses inside the
  posts, 36.6% inside the frame (impossible for a miss). Reverted to
  q102 (pannadata, 26 Nov); `backfill_goalmouth.py` overwrites Miss
  `goalmouth_y` from q102 on every daily run, which repairs the
  published rows. xGOT was unaffected (on-target shots only).
- **No working z/height (q103) replacement for misses is known**; q147
  is too sparse (~13.5% presence). That half of panna#175 remains open.

### Unconfirmed — do not guess, do the work first

- **Six-yard block** (`sixYardBlock`): no qualifier found that
  discriminates it from a plain block, even after restricting to
  confirmed q94 block events and checking event coordinates
  (six-yard-block events average x≈13.6, statistically indistinguishable
  from plain-block events at x≈13.8 — clearance-off-line events, by
  contrast, are cleanly separated at x≈4.9). Whatever marks this stat,
  it isn’t in the qualifier set checked here. Needs either a wider
  qualifier search or a different signal entirely (a paired event? a
  distinct sub-type of q94?). See the 2026-07-23 entry above for a
  related-but-distinct finding: the `sixYardBlock` STAT name
  specifically remains unresolved, but a blocked SHOT’s real block
  location is recoverable via its companion `type_id==10`+q94 event’s
  own x/y.
- **q14 (“Last man”) does not travel to domestic-league data.**
  Re-checked 2026-07-23 on EPL: q14 has 0% presence on `type_id==12`
  (Clearance) events across all 15 seasons (2011-12 → 2025-26),
  confirmed via a raw qualifier-key dump on 2,000 sampled events (14
  simply isn’t in the key set:
  `{15,21,56,138,140,141,167,178,185,189,212,213,233,388,389,397}`). The
  `clearanceOffLine` finding above was confirmed on World Cup data only
  — treat it as competition-scoped, not general, until re-checked on
  another domestic league.
- **Zone/coordinate qualifiers 55/56/102/103/230**: the prior version of
  this doc listed these as start/end coordinate qualifiers, but they
  conflict with 140/141 (confirmed above, and the qualifiers actually
  consumed by production code for end coordinates). Not re-verified this
  session — treat the prior claims here as unverified, not
  wrong-but-unconfirmed. (102 is now confirmed as goal-mouth y and
  230/231 as the keeper’s x/y — see above.)

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
