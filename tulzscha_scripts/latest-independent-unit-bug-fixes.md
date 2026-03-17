# Latest Independent Unit Bug Fixes

*Session: 2026-03-15*

---

## Bug 1: Y'Golonac Orifices — Infinite Loop on Skip

**Unit:** Y'Golonac (IGOO)
**File:** `solo/Battle.scala` — `YgolonacOrificesPhase`
**Symptom:** After Y'Golonac is killed in battle, the Orifices prompt (select a target to transfer Y'Golonac to) keeps reappearing in an infinite loop after pressing Skip.

**Root cause:** The Skip button was wired to `BattleDoneAction(s)`, which calls `proceed()`. `proceed()` re-evaluates the *current* phase. Since Y'Golonac is still in `forces` with `health == Killed` (elimination hasn't happened yet — that's `EliminatePhase`), the condition is still true and the Ask is presented again.

**Fix:**
```scala
// Before (infinite loop):
.skip(BattleDoneAction(s))

// After (jumps directly past the phase):
.skip(BattleProceedAction(EliminatePhase))
```

**Rule:** Never use `.skip(BattleDoneAction(s))` in a kill phase. Always use `.skip(BattleProceedAction(NextPhase))`. The `EternalKillPhase` uses an alternative: mutate state (e.g. `s.remove(Eternal)`) *before* the Ask so `proceed()` re-entry finds nothing to do. Either approach works; `BattleProceedAction` is simpler when you can't remove the unit from forces early.

---

## Bug 2: Y'Golonac Orifices — Replaced Unit Stays in Battle Forces

**Unit:** Y'Golonac (IGOO)
**File:** `solo/IGOOs.scala` — `YgolonacOrificesAction` handler
**Symptom:** The unit replaced by Y'Golonac (a Terror, Monster, or Cultist) was not properly removed from the battle's forces list after being eliminated.

**Root cause:** `game.eliminate(target)` does not call `exempt()`. Without exemption, the unit stays in the Side's `forces` list with its health reset to `Alive` after elimination. This means it can still receive pain assignments in later battle phases as if it were alive.

**Fix:**
```scala
// Before:
game.eliminate(target)

// After:
val targetFigure = game.unit(target)
game.battle.foreach(_.exempt(targetFigure))  // removes from forces, adds to exempted list
game.eliminate(targetFigure)
```

**Rule:** When eliminating a battle participant from *outside* `Battle.scala` (e.g. from an action handler in `IGOOs.scala`), always call `game.battle.foreach(_.exempt(figure))` before `game.eliminate(figure)`. `Battle.eliminate()` does this automatically when called from within `Battle.scala`.

---

## Bug 3: Online Game Crash — "Unknown symbol: GnorriCard"

**Units affected:** Gnorri, Tulzscha, Y'Golonac (and any future loyalty cards)
**File:** `solo/Serialize.scala`
**Symptom:** Crash with `Uncaught java.lang.IllegalArgumentException: Unknown symbol: GnorriCard` when starting an online (multiplayer) game. Does not appear in solo/local games.

**Root cause:** The online game serializes and deserializes game state between players. `Serialize.scala` maintains an explicit `loyaltyCards` list used by `parseLoyaltyCard()`. Any loyalty card not in this list causes the deserializer to throw "Unknown symbol".

**Fix:** Add every new loyalty card to the list in `Serialize.scala`:
```scala
val loyaltyCards = $(..., TulzschaCard, GnorriCard, YgolonacCard)
```

**Rule:** After adding any loyalty card (neutral monster or IGOO), always add its card object to `loyaltyCards` in `Serialize.scala`. Unit classes, spellbooks, and game options do *not* need this — they are resolved automatically by `parseSymbol` via reflection. Only loyalty cards require the explicit list entry.

Always test an online game after adding a new unit to catch this class of error, since it is invisible in local play.

---

## Bug 4: Online Undo Fails — "rollback-v2" 404

**File:** `online/CthulhuWarsOnline.scala`
**Symptom:** Pressing the undo button in an online game shows "undo failed, reloading" and the console shows `POST /rollback-v2/{hash}/{n} 404 (Not Found)`.

**Root cause:** The online server had no `rollback-v2` route. The client calls this endpoint to delete log entries beyond a given index (rolling back game state), but the server only had `create`, `roles`, `role`, `read`, and `write` routes.

**Fix:** Add the missing route to `online/CthulhuWarsOnline.scala` after the `write` route:

```scala
(post & path("rollback-v2" / Segment / IntNumber)) { (role, index) =>
    q(roles.filter(_.secret === role).map(_.gameId).result.head.flatMap { id =>
        logs.filter(_.gameId === id).filter(_.index >= index).delete
    })
    complete(StatusCodes.Accepted)
}
```

**Rule:** This is a server-only change — no JS rebuild needed, just restart the online server. Note that `online/CthulhuWarsOnline.scala` is a completely separate file from the solo JS. Undo-related bugs may require changes there rather than in `solo/`.

---

## Bug 5: Gnorri Summon Cost Showing 2 Instead of 3

**Unit:** Gnorri (Neutral Monster)
**File:** `solo/NeutralMonsters.scala`
**Symptom:** Gnorri showed a summoning cost of 2 Power in-game instead of the correct 3 Power.

**Root cause:** `UnitClass("Gnorri", Monster, 2)` — the third parameter of `UnitClass` is the summon cost, not the combat value. It was set to 2 (the combat value) instead of 3 (the summon cost). `GnorriCard` had `cost = 3` correctly, but the `UnitClass` definition governs what the game actually charges.

**Fix:**
```scala
// Before:
case object Gnorri extends UnitClass("Gnorri", Monster, 2) with NeutralMonster

// After:
case object Gnorri extends UnitClass("Gnorri", Monster, 3) with NeutralMonster
```

**Rule:** The third parameter of `UnitClass` is always the **summon cost**, not combat. Combat goes in `NeutralMonsterLoyaltyCard(..., combat = X)`. Always verify both match the intended values — they are easy to swap since combat and summon cost are often similar numbers.

---

## Bug 6: Tulzscha Awakened 4 Times in a Row — Duplicate `UseTulzscha` in Options

**Unit:** Tulzscha (IGOO)
**File:** `solo/Game.scala` — `UseTulzscha` option definition
**Symptom:** In an online multiplayer game, Opener was somehow able to awaken Tulzscha 4 consecutive times in the same action phase, ending up with 4 Tulzscha units on the map.

**Root cause:** `UseTulzscha` was missing the `with IGOOOption` trait:
```scala
case object UseTulzscha extends LoyaltyCardGameOption(TulzschaCard)  // missing IGOOOption!
```
When a player toggles the "Use IGOOs" group ON in setup, the code does:
```scala
setup.options ++= $(UseByatis, UseAbhoth, UseDaoloth, UseNyogtha, UseTulzscha, UseYgolonac)
```
This appends all options — including `UseTulzscha` — without deduplicating.

When the player toggles IGOOs OFF, the code does:
```scala
setup.options = setup.options.notOf[IGOOOption]
```
Since `UseTulzscha` lacked `IGOOOption`, it was NOT removed by this step.

Result: each ON/OFF/ON cycle of the IGOOs toggle accumulated one more copy of `UseTulzscha` in `setup.options`. After 4 cycles, `game.loyaltyCards` was initialized with 4 copies of `TulzschaCard`. The `independents()` function iterates all entries in `loyaltyCards`, generating one `IndependentGOOMainAction` per copy — so the menu showed 4 (identical-looking) Tulzscha awaken options and the player could awaken 4 times.

**Fix:**
```scala
// Before:
case object UseTulzscha extends LoyaltyCardGameOption(TulzschaCard)

// After:
case object UseTulzscha extends LoyaltyCardGameOption(TulzschaCard) with IGOOOption
```

**Side effect fixed:** Without `IGOOOption`, `options.of[IGOOOption].any.$(IGOOsExpansion)` would NOT include `IGOOsExpansion` when Tulzscha was the only enabled IGOO. The fix ensures the expansion is properly loaded in that case too.

**Rule:** Every `Use*` option for an IGOO must extend `IGOOOption`. This trait controls two behaviors: (1) inclusion in `IGOOsExpansion`; (2) cleanup when the IGOOs group is disabled. Omitting it causes both a broken solo experience (no expansion loaded) and a setup duplication bug when the IGOOs group is toggled on/off multiple times.

---

## Bug 7: Dimensional Shambler Killed → Returned to Faction Card Instead of Pool

**Unit:** Dimensional Shambler (Neutral Monster)
**File:** `solo/NeutralMonsters.scala`
**Symptom:** When a Dimensional Shambler was killed in battle, it was placed back onto the faction card (the holding area `ShamblerHold`) instead of being returned to the reserve pool.

**Root cause:** A prior session had added an `override def eliminate` to `NeutralMonstersExpansion` that explicitly sent killed Shamblers to `ShamblerHold(u.faction)` and reset their health to `Alive`. This was based on a misunderstanding of the rule — getting to the faction card requires an explicit summon action; death returns the unit to the pool like any other unit.

**Fix:** Remove the entire `override def eliminate` from `NeutralMonstersExpansion`. The default elimination behavior (unit returns to `faction.reserve`) is correct.

```scala
// Before (incorrect):
object NeutralMonstersExpansion extends Expansion {
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (u.uclass == DimensionalShamblerUnit) {
            u.remove(Eliminated)
            u.region = ShamblerHold(u.faction)
            u.health = Alive
            u.onGate = false
            u.state = $
        }
    }
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ { ... }
}

// After (correct):
object NeutralMonstersExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ { ... }
}
```

**Rule:** When a Dimensional Shambler dies, it returns to the reserve pool (same as any other monster). To get it back to the faction card, the player must spend power to summon it again via `ShamblerSummonAction`. Only the `ShamblerDeployAction` empty-guard (preventing a crash if `ShamblerHold` is somehow empty during deploy) is needed — no `eliminate` override.

---

## Bug 8: SummonAction / AwakenAction Execute With Insufficient Power — Negative Power State

**Files:** `solo/Game.scala` — `SummonAction` and `AwakenAction` handlers
**Symptom:** In an online multiplayer game, a faction summoned a unit twice in rapid succession, with the log showing: "summoned [Unit]" → "ran out of power" → "summoned [Unit]" → "somehow ran into negative power".

**Root cause investigation — undo mechanism:**

The undo (rollback-v2) mechanism was investigated as a potential cause. Analysis showed the math is correct:
- The client sends `(actions.num + 3)` as the rollback index
- The server deletes `filter(_.index >= index)`, which removes exactly the right entries
- After rollback, forced action chains (`SummonedAction → EndAction → AfterAction`) are **re-derived from game logic** on replay — they don't need to be in the DB
- Therefore, undo **cannot** produce `acted=false` for a faction whose summon is visible (EndAction always re-runs)

Undo CAN give a faction `acted=false` with power remaining if rolled back to mid-turn (before EndAction but after a summon) — but this is correct undo behavior, not a bug, and the summon is also replayed correctly.

**Actual root cause:** Online action processing is async. The action sequence:
1. Player clicks Summon (has power) → `SummonAction` written to DB
2. UI state not yet updated — game performs the action via DB read/replay cycle
3. In a stale-state or race-condition window, a second `SummonAction` could be written to the DB before the first had fully updated the power/acted state

Without a power check in the `SummonAction` handler itself, the action executes unconditionally, deducting power even if it's already 0, producing negative power. The same risk existed for `AwakenAction`.

**Fix — added affordability guards:**
```scala
// SummonAction — before (no guard):
case SummonAction(self, uc, r) =>
    self.power -= self.summonCost(uc, r)
    self.payTax(r)
    self.place(uc, r)
    self.log("summoned", uc.styled(self), "in", r)
    SummonedAction(self, uc, r, $)

// SummonAction — after (with guard):
case SummonAction(self, uc, r) =>
    if (self.pool(uc).none || self.affords(self.summonCost(uc, r))(r).not)
        EndAction(self)
    else {
        self.power -= self.summonCost(uc, r)
        self.payTax(r)
        self.place(uc, r)
        self.log("summoned", uc.styled(self), "in", r)
        SummonedAction(self, uc, r, $)
    }

// AwakenAction — before (pool check only):
case AwakenAction(self, uc, r, cost) =>
    if (self.pool(uc).none)
        EndAction(self)
    else { ... }

// AwakenAction — after (pool + afford check):
case AwakenAction(self, uc, r, cost) =>
    if (self.pool(uc).none || self.affords(cost)(r).not)
        EndAction(self)
    else { ... }
```

**Rule:** Any action handler that deducts power should guard with `affords` before doing so, not just rely on the menu builder's pre-check. The menu builder filters affordable options at display time, but in online games there is an async gap between when the player clicks and when the action executes — game state can change in that window. A server-side guard in the handler is the defensive line that prevents negative power regardless of how the action ended up in the DB.

---

## Bug 9: Dimensional Shambler Killed → Still Appears on Faction Card (True Root Cause)

**Unit:** Dimensional Shambler (Neutral Monster)
**Files:** `solo/NeutralMonsters.scala`, `solo/Game.scala`
**Symptom:** After Bug 7's fix (removing `override def eliminate`), killed Dimensional Shamblers still appeared to return to the faction card rather than the reserve pool.

**Root cause:** Three interrelated issues in the summon/acquisition logic:

1. **`NeutralMonstersAction` for Dimensional Shambler** only created 1 unit figure and placed it directly at `ShamblerHold`. The remaining 2 figures (quantity = 3) were never added to `self.units`. When that 1 figure was killed and correctly returned to `self.reserve`, the pool unit was visible in the faction card's pool rendering area — which the user perceived as "returned to faction card."

2. **`ShamblerSummonAction`** created a brand-new `UnitFigure` with `:+=` each time the player summoned a Shambler to the faction card, instead of moving an existing pool figure. Killed Shamblers accumulated in the pool while new (additional) figures were created at `ShamblerHold` on each summon.

3. **`ShamblerSummonMainAction` availability check** used total unit count (`f.units.%(_.uclass == DimensionalShamblerUnit).num < DimensionalShamblerCard.quantity`) instead of pool count. Because `ShamblerSummonAction` kept creating new figures, this count kept growing until the summon button stopped appearing even with pool units available.

**Fix — `NeutralMonstersAction`** (add all figures to reserve, move 1 to ShamblerHold):
```scala
// Before:
self.units :+= new UnitFigure(self, DimensionalShamblerUnit, 1, ShamblerHold(self))

// After:
lc.quantity.times(DimensionalShamblerUnit).foreach { u =>
    self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num + 1, self.reserve)
}
self.pool(DimensionalShamblerUnit).head.region = ShamblerHold(self)
```

**Fix — `ShamblerSummonAction`** (move pool figure, don't create new):
```scala
// Before:
self.units :+= new UnitFigure(self, DimensionalShamblerUnit, self.units.%(_.uclass == DimensionalShamblerUnit).num + 1, ShamblerHold(self))

// After:
self.pool(DimensionalShamblerUnit).head.region = ShamblerHold(self)
```

**Fix — `ShamblerSummonMainAction` availability** (Game.scala):
```scala
// Before:
f.units.%(_.uclass == DimensionalShamblerUnit).num < DimensionalShamblerCard.quantity

// After:
f.pool(DimensionalShamblerUnit).any
```

**Rule:** For a special-placement neutral monster (one where `canBeSummoned` is `false` and units are held in a dedicated `FactionRegion`), ALL figures must be added to the reserve pool at card acquisition time — just like a standard monster. The holding region is a transit point, not the home location. On summon, move an existing pool figure to the holding region; on death, the default `eliminate` returns it to the pool automatically. Never use `:+=` to create a new figure on each summon — that bypasses the pool entirely and breaks the entire lifecycle.

---

## Bug 10: Code Fix Has No Effect — `sbt fastLinkJS` Silently Serves Old JS

**Session:** 2026-03-16
**Symptom:** A code fix was correctly written and compiled with `sbt fastLinkJS` (reported success), but the bug persisted in-game with no change in behavior, even in an Incognito window.

**Root cause:** `sbt fastLinkJS` produces a directory-format output (`cthulhu-wars-solo-hrf-fastopt/`). The `index.html` references a single `.js` file and cannot load this format. The build succeeds but the browser silently continues serving the previous `fastOptJS` output unchanged.

**Fix:** Run `sbt fastOptJS` — no clean needed, it overwrites the correct output file:
```bash
cd ~/cthulhu-wars/solo && sbt fastOptJS
```

**Rule:** Always use `sbt fastOptJS`. Never use `sbt fastLinkJS`. The two commands produce different output formats; `fastLinkJS` success is a false positive in this project. If a fix appears to have no effect after a successful build, the first thing to check is which sbt command was used.

---

## Bug 11: Dimensional Shambler Deploy — Spellbook Not Awarded Immediately

**Unit:** Dimensional Shambler (Neutral Monster)
**File:** `solo/Game.scala` — `PreActionPromptsAction` section, line where `ShamblerDeployPromptAction` is created
**Symptom:** When deploying the Dimensional Shambler to a location that fulfills a spellbook requirement (e.g. Opener of the Way's "Units at two enemy Gates"), the spellbook was not awarded immediately. The game logged "achieved Units at two enemy Gates", then proceeded to the next player's turn, and only then awarded the spellbook.

**Root cause:** Two separate issues combine to cause the delay:

1. The out-of-turn deploy prompt is created with `then = PreMainAction(e)` — no `CheckSpellbooksAction` in the chain at all.

2. Even after adding `CheckSpellbooksAction(PreMainAction(e))` as the `then`, the spellbook still isn't awarded immediately. `CheckSpellbooksAction` checks `f.unfulfilled`, but `unfulfilled` is only populated when `triggers()` runs. `triggers()` is called inside `checkGatesGained()`, which is called by `AfterAction` — but the Shambler deploy is an out-of-turn action, not a main action, so no `AfterAction` fires for it. The deploy completes, `CheckSpellbooksAction` finds `unfulfilled` empty, and passes through. The condition is only evaluated when the next player's `AfterAction` calls `checkGatesGained` → `triggers()`, which is why the spellbook appears after the next player's turn.

**Full fix — two changes:**

`solo/Game.scala` — wrap the deploy prompt's `then` with `CheckSpellbooksAction`:
```scala
// Before:
ShamblerDeployPromptAction(f, PreMainAction(e))

// After:
ShamblerDeployPromptAction(f, CheckSpellbooksAction(PreMainAction(e)))
```

`solo/NeutralMonsters.scala` — call `game.triggers()` before returning `then` in `ShamblerDeployAction`, so conditions are evaluated before `CheckSpellbooksAction` runs:
```scala
// Before:
else
    then

// After:
else {
    game.triggers()
    then
}
```

**⚠️ UNTESTED** — both changes are in the source but have not been built and verified in-game yet.

**Rule:** Any out-of-turn unit placement must (1) chain through `CheckSpellbooksAction` as the continuation, AND (2) call `game.triggers()` immediately before that continuation fires, so that spellbook conditions reflect the new board state. `CheckSpellbooksAction` alone is not sufficient — it reads `f.unfulfilled` which is only populated by `triggers()`. Normal main actions get this for free via `AfterAction` → `checkGatesGained` → `triggers()`; out-of-turn actions must do it explicitly.

---

## Bug 12: Ceremony of Annihilation — Sleeper's Doom Turn Skipped

**Unit:** Tulzscha (IGOO) / Opener of the Way
**File:** `solo/IGOOs.scala` — `CeremonyOfAnnihilationChoiceAction` class definition
**Symptom:** When Opener uses Ceremony of Annihilation in the doom phase, the next faction's (Sleeper's) doom turn is skipped entirely.

**Root cause:** `CeremonyOfAnnihilationChoiceAction extends OptionFactionAction(...) with DoomQuestion with Soft`. The `Soft` trait means the action is NOT written to the online DB. Only the forced follow-up `DoomAction(Opener)` (the second doom phase re-entry) makes it to the DB.

The result: Opener's client has `Opener.acted = true` (set by ceremony). Other clients replay `DoomAction(Opener)` at position N from the DB — but the DB does not contain the ceremony action. From their perspective, `Opener.acted = false` when the second doom phase ask appears. This desync means both clients are showing Opener's doom phase, but with different game state. The `DoomDoneAction(Opener)` effectively fires out of sync across clients, causing one client to rotate factions twice — skipping Sleeper's doom turn.

The normal `RitualAction` does NOT have this bug because it is a `BaseFactionAction` (not Soft), so it IS recorded to the DB. Both clients see it, both have `Opener.acted = true` when the second `DoomAction(Opener)` fires, and everything stays in sync.

**Fix — `solo/IGOOs.scala`** — remove `with Soft` from `CeremonyOfAnnihilationChoiceAction`:
```scala
// Before (Soft — not written to DB, causes client desync):
case class CeremonyOfAnnihilationChoiceAction(self : Faction) extends OptionFactionAction(
    g => "Use " + CeremonyOfAnnihilation.styled(self) + " (earn " + g.ritualCost.power + ", no Doom/ES)"
) with DoomQuestion with Soft

// After (recorded — both clients apply ceremony before the second DoomAction):
case class CeremonyOfAnnihilationChoiceAction(self : Faction) extends OptionFactionAction(
    g => "Use " + CeremonyOfAnnihilation.styled(self) + " (earn " + g.ritualCost.power + ", no Doom/ES)"
) with DoomQuestion
```

**Rule:** Any action that modifies game state (power, `acted` flag, spellbook conditions) must NOT be `Soft` in an online game. `Soft` is appropriate only for pure UI navigation actions (like opening a sub-menu) that don't change game state. Actions that call `self.acted = true`, grant power, or satisfy spellbook requirements must be recorded so all clients have consistent state.

---

## Bug 13: Action Phase Double Turn — 409 Conflict on DB Write

**File:** `solo/CthulhuWarsSolo.scala` — online action write handler
**Symptom:** A player moves a unit and clicks Done, but instead of advancing to the post-action menu (controls/battles/end-turn), the game returns to their full main action menu as if they haven't acted. 409 Conflict errors appear in the browser console.

**Root cause:** In online games, when a player clicks an action (e.g. `MoveAction`), the client writes the action to the DB at position `actions.num + 3`. If two clients try to write to the same position simultaneously (race condition), the server returns 409 Conflict.

The current 409 handler reads whatever is already at that position from the DB, then does `UIRead(g)` — but `g` (the local game object) has ALREADY had the attempted action applied locally before the write was attempted. When `UIRead` replays the DB's actual action on the already-mutated `g`, the resulting game state is corrupted: counters may be applied twice, `acted` flags may be in the wrong state, or the action history gets misaligned.

In the observed case, Opener submitted a move but got a 409. The retry logic replayed the DB's version, but `g` had both the attempted move AND the DB's replay applied — resulting in `Opener.acted` being reset or not set properly, showing the main action menu again.

**No clean code fix found** — this is a fundamental async race condition in the online DB write architecture. Current mitigation: page reload brings both clients back in sync. The proper fix would require either:
- Server-side write transactions (lock position, reject duplicates, return current state)
- Client-side game state snapshot + rollback before the retry read

**Workaround:** If a 409 occurs mid-game, both affected players should reload the page. The DB is the source of truth; reloading replays all recorded actions and restores consistent state.

---

## Bug 14: Opener Double Turn in Action Phase

**Unit:** Opener of the Way (with Dimensional Shambler + Tulzscha)
**Files:** Suspected: `solo/CthulhuWarsSolo.scala` (online state desync), `solo/IGOOs.scala` (ceremony Soft bug)
**Symptom:** Later in the same action phase where Bugs 12 and 13 occurred, Opener was allowed to take two action phase turns in a row.

**Theory:** This is likely a downstream consequence of the state desync from Bug 12 (Ceremony) and/or Bug 13 (409 Conflict). Both bugs can corrupt `acted` flags:
- Bug 12's desync may leave one client with `Opener.acted = false` in a state where the other client has `Opener.acted = true`. If the bug-12-affected client drives action resolution, it may present Opener's main action menu when `acted` should already be `true`.
- Bug 13's 409 corruption (see above) could similarly reset `Opener.acted` to `false` mid-turn if the corrupted game state is missing the `EndAction` that sets it.

The Dimensional Shambler and Tulzscha are not themselves causing the extra turn — they add out-of-turn prompts (`ShamblerDeployPromptAction`, `TulzschaGivePowerMainAction`) but all of these use correct continuations that do not reset `acted` or re-enter the main action phase for Opener.

**Expected resolution:** Fixing Bug 12 (ceremony Soft → recorded) eliminates the primary state desync vector. If double turns persist after that fix, investigate whether Dragon Ascending or Shambler out-of-turn deploy is somehow resetting `Opener.acted` in an edge case not yet observed.

---

## Bug 15: Shambler Command Settings Menu Visible When No Shambler Is on Faction Card

**Unit:** Dimensional Shambler (Neutral Monster)
**Files:** `solo/Game.scala` — `extraActions` and `CommandsMainAction(f)` handler
**Symptom:** The "Dimensional Shambler" command settings group (ShamblerPrompt / Skip / threat options) remained visible in the out-of-turn Commands panel even when no Dimensional Shambler was on the faction card (i.e. all shamblers were on the map or in the reserve pool). For comparison, Devolve and Dragon Ascending prompts correctly disappear when their actions cannot currently be taken.

**Root cause:** Two places in `Game.scala` iterated `f.plans` without filtering for whether the plans are currently actionable:

1. `extraActions(f, ...)` — builds the info display and `CommandsMainAction(f)` button shown in the out-of-turn section. It used `f.plans.any.$(CommandsMainAction(f))` unconditionally, showing the button whenever any plans existed.

2. `CommandsMainAction(f)` handler — iterated `f.plans` directly, showing all plan groups in the settings menu regardless of whether the underlying action was possible.

The `ShamblerDeployPromptAction` itself is correctly gated by `f.at(ShamblerHold(f), DimensionalShamblerUnit).any` in `PreActionPromptsAction`, but the command settings for WHEN to deploy were not gated by the same condition.

**Fix — `solo/Game.scala`** — filter `ShamblerPlan`s out of visible plans in both locations when no Shambler is in `ShamblerHold`:

`extraActions`:
```scala
// Before:
f.plans.%(f.commands.has)./(p => Info(p.info)(p.group)) ++
f.plans.any.$(CommandsMainAction(f))

// After:
{ val vp = f.plans.%(p => p.is[ShamblerPlan].not || f.at(ShamblerHold(f), DimensionalShamblerUnit).any)
vp.%(f.commands.has)./(p => Info(p.info)(p.group)) ++
vp.any.$(CommandsMainAction(f)) }
```

`CommandsMainAction(f)` handler:
```scala
// Before:
Ask(f).each(f.plans) { p =>

// After:
val visiblePlans = f.plans.%(p => p.is[ShamblerPlan].not || f.at(ShamblerHold(f), DimensionalShamblerUnit).any)
Ask(f).each(visiblePlans) { p =>
```

**Rule:** Command settings groups for a unit ability should be gated by the same condition that gates the ability's actual prompt. For the Shambler, that condition is `f.at(ShamblerHold(f), DimensionalShamblerUnit).any` — if no Shambler is on the faction card, neither the deploy prompt nor its configuration settings should be visible. Any new out-of-turn ability with plan-based settings must apply a matching visibility filter in both `extraActions` and the `CommandsMainAction` handler.

---

## Bug 16: Shambler Summon Menu — Wrong Coloring and Missing Power Cost

**Unit:** Dimensional Shambler (Neutral Monster)
**File:** `solo/NeutralMonsters.scala` — `ShamblerSummonMainAction` and `ShamblerSummonAction` class definitions
**Symptom:** The "Summon Dimensional Shambler to Faction Card" top-level menu entry showed "Dimensional Shambler" in neutral color instead of the faction's color. The sub-menu item after clicking it showed the same wrong color, had no power cost displayed, and "Faction Card" was not styled with faction color. All other unit summon actions display correctly.

**Root cause:** Both action classes used `DimensionalShamblerUnit.name.styled("neutral")` (hardcoded neutral color) instead of `DimensionalShamblerUnit.styled(self)` (faction color via `self`'s style). `ShamblerSummonAction` also used a plain string first argument instead of an `implicit g =>` lambda, so `forNPowerWithTax` could not be called and no power cost appeared.

**Fix — `solo/NeutralMonsters.scala`:**
```scala
// Before:
case class ShamblerSummonMainAction(self : Faction) extends OptionFactionAction(
    "Summon " + DimensionalShamblerUnit.name.styled("neutral") + " to Faction Card") with MainQuestion with Soft
case class ShamblerSummonAction(self : Faction) extends BaseFactionAction(
    "Summon " + DimensionalShamblerUnit.name.styled("neutral"),
    "to Faction Card".styled("neutral"))

// After:
case class ShamblerSummonMainAction(self : Faction) extends OptionFactionAction(
    "Summon " + DimensionalShamblerUnit.styled(self) + " to Faction Card") with MainQuestion with Soft
case class ShamblerSummonAction(self : Faction) extends BaseFactionAction(
    implicit g => "Summon " + DimensionalShamblerUnit.styled(self) + g.forNPowerWithTax(self.reserve, self, self.summonCost(DimensionalShamblerUnit, self.reserve)),
    "to " + "Faction Card".styled(self))
```

**Rule:** Any action class that displays a unit name in a menu must use `uc.styled(self)` (faction color), not `uc.name.styled("neutral")` (hardcoded neutral). To include a power cost in a `BaseFactionAction` header, use `implicit g => "..." + g.forNPowerWithTax(r, self, cost)` — this requires the `implicit g =>` lambda form even if the action has no region parameter (use `self.reserve` as the region when the destination is the faction card). `forNPowerWithTax` includes tax automatically and formats the power in white via `.power` styling.

---

## Bug 17: MFO Spellbook Skipped in Online Battle When Opener Has Shambler on Faction Card

**Unit:** Dimensional Shambler (Neutral Monster) / Opener of the Way
**File:** `solo/CthulhuWarsSolo.scala` (online async write handler) — same architecture as Bug 13
**Symptom:** In an online game, Opener (with the Dimensional Shambler on the faction card) enters battle. The battle completes with surviving promotable units, but the "Million Favored Ones" (MFO) spellbook prompt never appears. Performing undo back to just before the battle and replaying causes MFO to fire correctly the second time.

**Root cause:** This is a Bug 13-type 409 Conflict race condition triggered specifically by the Shambler's out-of-turn deploy.

The key sequence:
1. Before the enemy's action, `PreActionPromptsAction` runs for Opener. If Opener has a Shambler on the faction card and the deploy condition is met, `ShamblerDeployPromptAction` fires (Soft — not recorded, only on Opener's client), then `ShamblerDeployMainAction` / `ShamblerDeployAction`.
2. `ShamblerDeployAction` IS a recorded `BaseFactionAction` — it writes to the DB at position `actions.num + 3`.
3. In async mode, the enemy may simultaneously be writing an action to the same DB position.
4. One client gets a 409 Conflict. The retry reads the existing DB entry and applies it to the local game object `g` — but `g` has ALREADY been mutated locally by the attempted action. The result is a corrupted game state.
5. The corrupted state propagates into the battle. Specifically, `attacker.forces` or the side's tag state becomes inconsistent. `preroll(s)` is called with the corrupted state, and the `if (s.has(MillionFavoredOnes)) s.add(MillionFavoredOnes)` tag is either never reached or the side state is in an unexpected shape. MFO is never tagged, so `PostBattlePhase` finds no MFO to process.

**Why undo/redo fixes it:** Undo replays the entire action sequence from the DB from scratch into a fresh game state, bypassing the corrupted `g`. The second playthrough has no race condition, `preroll` tags MFO correctly, and `PostBattlePhase` fires it.

**Why the Shambler is specifically susceptible:** The `ShamblerDeployAction` fires during `PreActionPromptsAction` — which is exactly the window in async mode where the enemy is most active. The Shambler adds an extra recorded write in the one window where both players are simultaneously allowed to write.

**No clean code fix found** — same fundamental limitation as Bug 13. The 409 retry handler in `CthulhuWarsSolo.scala` applies the DB action to an already-mutated `g`, producing corruption. The proper fix requires either server-side write transactions or a client-side snapshot+rollback before the retry.

**Workaround:** Undo back to just before the battle (or back to just before the Shambler deploy) and replay. The DB is the source of truth; replaying from it restores consistent state.
