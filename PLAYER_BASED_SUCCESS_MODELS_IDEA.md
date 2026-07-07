# Idea: player-aware (matchup) variants of the "success / above-expected" models

> **Status: future work.** Captured 2026-06-24 while building the player-agnostic
> expected-duel model. Ship the player-agnostic versions first; this is the v2.

## The gap

Every contextual model in panna is currently **player-agnostic** — features are
context only (location, action type, distance/angle, period), never the identity
of the actor or the opponent:

| model | file | target | currently uses |
|-------|------|--------|----------------|
| xG | `xg_model.R` | P(goal \| shot) | shot context |
| xGOT | `xgot_model.R` | P(goal \| on-target shot, placement) | placement context |
| xPass | `xpass_model.R` | P(completion) | pass geometry |
| **xDuel (aerial)** | `duel_model.R` *(new)* | P(win aerial) | location, period |
| **xTackle** | `duel_model.R` *(new)* | P(tackle success) | location, period |

"Above expected" from a player-agnostic model still **conflates skill with
schedule**: winning 60% of aerials means more if they were contested against
strong aerial opponents than weak ones. The agnostic model can't tell.

## The opportunity — we already have the opponent

For the contest models the **opponent is recorded**, so a matchup model is feasible:

- **Aerials (Opta type 44)**: both participants get a row (balanced 50/50), and
  SPADL carries `opponent_player_id` after `merge_duel_rows()`. Every aerial is a
  clean A-vs-B pair → a Bradley-Terry / Elo-style "aerial ability" is directly
  estimable.
- **Tackles (type 7)**: tackler + dribbler (`opponent_player_id`); cross-type
  duel detection already pairs take-on-fail ↔ tackle-success.
- **Shots → xGOT/GSAA**: the keeper is the opponent (already cross-attributed in
  `.compute_keeper_gsaa()`); a shooter-vs-keeper placement model is the natural
  matchup extension. **Worked example (user's):** GSAA today = `xGOT_faced −
  goals_conceded`, where `xGOT` is shooter-agnostic — so a keeper is "expected" to
  save the same share of a given on-target placement whether **Messi** or a
  **centre-back** struck it. Reality: an elite finisher's on-target shot is
  closer to unsaveable, a defender's is more saveable. A **shooter-aware xGOT**
  (`P(goal | placement, shooter_finishing_rating)`) would *raise* expected-goals
  for Messi's shot (so the keeper isn't penalised for conceding the near-
  unstoppable one) and *lower* it for the defender's (so failing to save it is a
  real miss). Net: keeper GSAA stops rewarding/penalising keepers for *who* shot
  at them. Mirror term on the attacking side = "finishing above a keeper-agnostic
  baseline," opponent-(keeper-)adjusted.
- **Passes (xPass)**: receiver is known; defensive pressure is the missing
  opponent term (no defender-proximity feature yet).

## Proposed design — two models per metric, agnostic + aware

1. **Player-agnostic (context only)** — what we're shipping now. Stays the
   production "above expected" feed; no leakage, dense, stable.
2. **Player-aware (matchup)** — add jointly-estimated **actor ability** +
   **opponent ability** terms (ridge/Elo/mixed-effects random effects per player),
   shrunk hard for sparsity. Output:
   - `*_woe_agnostic` (vs an average opponent) and
   - `*_woe_vs_opponent` (controlling for who they faced).
   - The **difference** = a "strength of schedule" term for that skill.

### Circularity guardrail
Do **not** feed a player's *own* identity into the model used to *rate that same
player* (it would predict their skill from their skill). The actor term is for
**credit/context only**; for the rating, use the opponent term (schedule control)
and keep the actor's own residual as the skill signal. Same discipline as RAPM:
control for everyone else, read off the player's own coefficient.

### Estimation sketch (aerials, cleanest case)
- Logistic with context features + `f(actor_aerial_rating) − f(opp_aerial_rating)`.
- Solve as iterative Elo (online) or a single ridge-penalised GLM with
  per-player dummies (offline, shrunk). Bradley-Terry is the closed-form analogue.
- "Aerials won above expected, opponent-adjusted" = Σ won − Σ P(win \| opponent).

## Risks / open questions
- **Sparsity**: most player pairs meet a handful of times → heavy shrinkage,
  minutes/contest-count minimum before trusting the aware value.
- **Cross-league comparability**: tie player ratings through the same co-occurrence
  network used for league offsets (`build_league_network()`), or anchor to Big-5.
- **Compute**: per-player random effects across ~1.6M duels is a bigger fit than
  the agnostic XGBoost — batch offline, cache ratings, refresh weekly.
- **Where it plugs in**: same `aggregate_player_xmetrics()` seam; emit the extra
  `*_woe_vs_opponent` columns alongside the agnostic ones so PSR/PSV can pick.

## Related: a take-on (offensive duel) model — don't merge with tackles

Tempting to pair take-on (Opta type 3, attacker) with tackle (type 7, defender)
into one two-sided "ground duel" the way aerials pair winner+loser — it would be
zero-sum. **Don't**: the two sides are *different skills* (dribbling vs tackling),
so one merged number can't route cleanly to OSR (dribbler) vs DSR (tackler).
Instead add a **separate take-on model** → an OFFENSIVE "beats-his-man above
expected" feature, alongside the DEFENSIVE tackle WOE. They're naturally linked
(`P(take-on success) ≈ 1 − P(tackle success)` at a location) but stay distinct so
the O/D decomposition holds. (Aerials are symmetric — both jump for the header —
which is why the winner/loser pairing IS correct there.) Note: tackle WOE is
NOT zero-sum on its own (one-sided action), which is fine and expected; the
per-type calibration keeps `Σ tackle_woe ≈ 0`.

## Scope when picked up
Do all five success models together (xG, xGOT, xPass, xDuel, xTackle) so the
agnostic-vs-aware pair is consistent across the metric family, rather than
one-off per model.
