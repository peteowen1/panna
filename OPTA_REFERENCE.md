# Opta Event Reference

Reference for Opta F24 event data type IDs and qualifier IDs used in the
panna EPV pipeline.

## Event Type IDs (`type_id`)

Events are classified by `type_id`. The `outcome` field indicates
success (1) or failure (0).

### Ball Events

| type_id | Event          | Description                                       |
|---------|----------------|---------------------------------------------------|
| 1       | Pass           | Any pass attempt                                  |
| 2       | Offside Pass   | Pass that results in offside                      |
| 3       | Take On        | Dribble attempt                                   |
| 4       | Foul           | Foul committed                                    |
| 5       | Out            | Ball out of play                                  |
| 6       | Corner Awarded | Corner kick awarded                               |
| 7       | Tackle         | Tackle attempt                                    |
| 8       | Interception   | Interception                                      |
| 9       | Turnover       | Loss of possession (not from tackle/interception) |
| 10      | Save           | Goalkeeper save                                   |
| 11      | Claim          | Goalkeeper claims cross                           |
| 12      | Clearance      | Defensive clearance                               |
| 13      | Miss           | Shot off target                                   |
| 14      | Post           | Shot hits post/crossbar                           |
| 15      | Attempt Saved  | Shot saved by goalkeeper                          |
| 16      | Goal           | Goal scored                                       |

### Other Events

| type_id | Event                        | Description                               |
|---------|------------------------------|-------------------------------------------|
| 17      | Card                         | Yellow or red card                        |
| 18      | Player Off                   | Player leaves pitch (injury/substitution) |
| 19      | Player On                    | Player enters pitch                       |
| 27      | Start                        | Period start                              |
| 28      | End                          | Period end                                |
| 30      | End (14)                     | End of first half                         |
| 32      | Start (2)                    | Start of second half                      |
| 34      | Team Set Up                  | Formation/lineup info                     |
| 35      | Player Changed Position      | Tactical position change                  |
| 36      | Player Changed Jersey        | Jersey number change                      |
| 37      | Collection End               | Data collection ended                     |
| 40      | Formation Change             | Team formation change                     |
| 41      | Punch                        | Goalkeeper punch                          |
| 42      | Good Skill                   | Skillful play                             |
| 43      | Deleted Event                | Event removed from feed                   |
| 44      | Aerial                       | Aerial duel                               |
| 45      | Challenge                    | 50/50 challenge                           |
| 49      | Ball Recovery                | Regaining possession                      |
| 50      | Blocked Pass                 | Pass blocked                              |
| 51      | Delay of Play                | Time wasting                              |
| 52      | Keeper Pick-up               | Goalkeeper picks up ball                  |
| 53      | Chance Missed                | Big chance not converted                  |
| 54      | Ball Touch                   | Simple ball touch                         |
| 55      | Temp Goal                    | Temporary goal marker                     |
| 56      | Resume Play                  | Play resumes                              |
| 57      | Contentious Referee Decision | Controversial call                        |
| 61      | Ball Touch                   | Another ball touch type                   |
| 67      | Offside                      | Offside called                            |
| 68      | Offside Provoked             | Defensive offside trap                    |
| 70      | Shield Ball Opp              | Shielding ball from opponent              |
| 74      | Injury Clearance             | Clearance due to injury                   |

### Shot Type IDs (13-16)

| type_id | Shot Result | SPADL Mapping                            |
|---------|-------------|------------------------------------------|
| 13      | Saved       | action_type = “shot”, result = “fail”    |
| 14      | Hit Post    | action_type = “shot”, result = “fail”    |
| 15      | Missed      | action_type = “shot”, result = “fail”    |
| 16      | Goal        | action_type = “shot”, result = “success” |

------------------------------------------------------------------------

## Qualifier IDs

Qualifiers provide additional context for events. The JSON format is:

``` json
{"qualifierId": value, "qualifierId2": value2, ...}
```

Where keys are qualifier IDs (as strings) and values are the qualifier
values (or null).

### Pass Qualifiers

| qualifier_id | Qualifier              | Description            |
|--------------|------------------------|------------------------|
| 1            | Long Ball              | Long pass              |
| 2            | Cross                  | Cross into box         |
| 3            | Head Pass              | Headed pass            |
| 4            | Through Ball           | Through ball           |
| 5            | Free Kick Taken        | Pass from free kick    |
| 6            | Corner Taken           | Pass from corner       |
| 7            | Players Caught Offside | Offside trap triggered |
| 107          | Throw In               | Throw-in pass          |
| 124          | Goal Kick              | Goal kick pass         |
| 155          | Chipped                | Chipped ball           |

### Shot Qualifiers

| qualifier_id | Qualifier        | Description                                |
|--------------|------------------|--------------------------------------------|
| 15           | Head             | Headed shot                                |
| 72           | Right Foot       | Right-footed shot                          |
| 73           | Left Foot        | Left-footed shot (implied if not 72 or 15) |
| 26           | Direct Free Kick | Shot from direct free kick                 |
| 9            | Penalty          | Penalty kick                               |
| 214          | Big Chance       | Big chance (high xG opportunity)           |
| 28           | **Own Goal**     | Own goal scored                            |

### Position/Zone Qualifiers

| qualifier_id | Qualifier              | Description                    |
|--------------|------------------------|--------------------------------|
| 55           | Relative Event Start X | Start X coordinate (0-100)     |
| 56           | Zone                   | Pitch zone (Left/Center/Right) |
| 102          | Start X                | Absolute start X               |
| 103          | Start Y                | Absolute start Y               |
| 230          | End X                  | Absolute end X (passes/shots)  |
| 231          | End Y                  | Absolute end Y                 |

### Timing Qualifiers

| qualifier_id | Qualifier       | Description             |
|--------------|-----------------|-------------------------|
| 374          | Event Timestamp | Full timestamp of event |
| 375          | Event Time      | Time string (MM:SS.mmm) |

### Other Important Qualifiers

| qualifier_id | Qualifier          | Description                      |
|--------------|--------------------|----------------------------------|
| 17           | Assisted           | Had an assist                    |
| 20           | Right to Left      | Attack direction                 |
| 21           | Involved           | Player involved in play          |
| 22           | Related Event ID   | Links to related event           |
| 24           | Intentional Assist | Deliberate assist                |
| 25           | Key Pass           | Chance-creating pass             |
| 29           | Blocked            | Shot/pass blocked                |
| 76           | Six Yard Block     | Blocked on goal line             |
| 77           | Saved Off Line     | Cleared off line                 |
| 78           | Other Body Part    | Not head/foot                    |
| 79           | Overrun            | Lost ball while dribbling        |
| 80           | Completed          | Action completed successfully    |
| 81           | Kept Possession    | Retained possession after action |
| 108          | Out of Play        | Ball went out                    |
| 136          | Second Yellow      | Second yellow = red              |
| 154          | Involved           | Player involvement flag          |
| 178          | Angle              | Shot angle                       |
| 280          | Minute             | Event minute                     |
| 281          | Second             | Event second                     |
| 282          | Player ID          | Player identifier                |
| 395          | Away Team ID       | Away team reference              |
| 396          | Home Team ID       | Home team reference              |

------------------------------------------------------------------------

## Outcome Field

| outcome | Meaning                                                     |
|---------|-------------------------------------------------------------|
| 1       | Success (pass completed, tackle won, shot on target, etc.)  |
| 0       | Failure (pass incomplete, tackle lost, shot blocked/missed) |

**Note:** For shots (type_id 13-16), the type_id itself indicates
outcome: - 16 = Goal (success) - 13, 14, 15 = Not goal (fail)

------------------------------------------------------------------------

## SPADL Mapping

The
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
function maps Opta events to standardized SPADL actions:

| Opta type_id   | SPADL action_type |
|----------------|-------------------|
| 1              | pass              |
| 2              | pass (offside)    |
| 3              | take_on           |
| 4              | foul              |
| 7              | tackle            |
| 8              | interception      |
| 10             | keeper_save       |
| 12             | clearance         |
| 13, 14, 15, 16 | shot              |
| 41             | keeper_punch      |
| 44             | aerial            |
| 49             | ball_recovery     |
| 50             | dispossessed      |
| 52             | keeper_pick_up    |
| 53             | keeper_claim      |
| 61             | ball_touch        |

------------------------------------------------------------------------

## Duel Events (type_id 44, 7)

Opta records both participants in a duel as separate consecutive
events: - Row N: Player A (Team 1) - outcome indicates if they won - Row
N+1: Player B (Team 2) - outcome indicates if they won

The
[`merge_duel_rows()`](https://peteowen1.github.io/panna/reference/merge_duel_rows.md)
function combines these into single rows: - Keeps the winner’s row
(outcome = 1) - Adds `opponent_player_id` and `opponent_player_name`
from loser’s row

------------------------------------------------------------------------

## Own Goals

Own goals are identified by **qualifier 28** on goal events (type_id =
16): - `is_own_goal = TRUE` when qualifier 28 is present - Player listed
is the one who scored against their own team - EPV delta =
`-1 - prev_epv` (opponent achieved maximum value)

------------------------------------------------------------------------

## Coordinate System

Opta uses 0-100 scale for both X and Y: - **X**: 0 = own goal line, 100
= opponent goal line - **Y**: 0 = right touchline (from attacking
perspective), 100 = left touchline - **Center**: (50, 50) - **Goal
center**: (100, 50) - **Penalty spot**: approximately (88, 50) -
**6-yard box**: X \> 94, Y between ~37-63 - **18-yard box**: X \> 83, Y
between ~21-79

The
[`normalize_spadl_coordinates()`](https://peteowen1.github.io/panna/reference/normalize_spadl_coordinates.md)
function ensures teams always attack toward X = 100.

------------------------------------------------------------------------

## References

- Opta F24 Feed Documentation (proprietary)
- SPADL Paper: <https://arxiv.org/abs/1802.07127>
- socceraction library: <https://github.com/ML-KULeuven/socceraction>
