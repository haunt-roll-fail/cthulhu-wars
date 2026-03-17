# Adding a New Neutral Monster or IGOO to Cthulhu Wars Online

*Covers both Neutral Monsters (Gnorri, Voonith, Dimensional Shambler, etc.) and Independent Great Old Ones (Tulzscha, Byatis, Abhoth, etc.)*

---

## Before You Start

### Define the card's stats

**For a Neutral Monster:**
- Name and Scala object name (use a suffix if ambiguous, e.g. `DimensionalShamblerUnit`)
- Quantity (how many figures)
- Loyalty Card Doom Cost — always 2 for neutral monsters (hardcoded in `NeutralMonsterLoyaltyCard`)
- Summon Cost (power paid per figure) — the `UnitClass` third parameter
- Combat Value — goes in `NeutralMonsterLoyaltyCard`
- Special Ability: name, phase, description
- Placement: Standard (place at gate) or Special (faction card, like Dimensional Shambler)

**For an IGOO:**
- Name and Scala object name
- Awaken Cost (power) and Awaken Requirements
- Combat Value — goes in `IGOOLoyaltyCard`
- Special Ability: name, phase, description
- Spellbook Requirement (what triggers earning the spellbook)
- Spellbook Ability: name, phase, description

### Image files needed
Both types need two image files in `solo/webp/`:
- `solo/webp/images/n-[name].webp` — board token (drawn on map and on faction card during GC submerge)
- `solo/webp/info/n-[name].svg` — info card icon (shown in loyalty card popup and as faction card icon)

Naming: `n-` + unit name in lowercase with spaces replaced by hyphens (e.g. `n-dimensional-shambler`).

**Check actual image pixel dimensions before writing DrawRect values:**
```bash
identify solo/webp/images/n-gnorri.webp   # ImageMagick
# or
sips -g pixelWidth -g pixelHeight solo/webp/images/n-gnorri.webp
```

---

## Key Concepts: DrawRect and the Faction Card

### DrawRect parameters
```scala
DrawRect(imageId, tint, x_offset, y_offset, width, height)
```
- `x_offset` = distance from center point to left edge → use `-(width / 2)` to center horizontally
- `y_offset` = distance from center point to top edge → use `-(height - 10)` to anchor bottom ~10px below center

### Faction card vs. map sizing
For units larger than about 70×85px, use a conditional to render at a smaller size on the faction card:
```scala
case Gnorri => if (region != null) DrawRect("n-gnorri", |(tint), x - 50, y - 90, 100, 100)  // map
                else               DrawRect("n-gnorri", |(tint), x - 30, y - 54, 60, 60)     // faction card
```
- `region != null` means the unit is on the map
- `region == null` means it's being drawn on the faction card (GC Deep display)

If the image is already a moderate size (≤70×85px or so), a single DrawRect without the conditional is fine.

### The `dd()` function renders at specified width/height
The `dd()` function on the faction card uses 5-arg `drawImage`, so the `width` and `height` in DrawRect ARE respected. You can (and should) scale down large images for the faction card.

### y_offset formula
`y_offset = -(height - 10)` puts the bottom of the image 10px below the center point, matching all other units. For a 60px-tall faction card image: `y - 50`. For 57px: `y - 47` (slight variation is fine; tune visually).

---

## Files to Change

### 1. `solo/NeutralMonsters.scala` (Neutral Monsters only)

Add three objects (four if special placement):

```scala
case object GnorriCard extends NeutralMonsterLoyaltyCard(GnorriIcon, Gnorri, cost = 3, quantity = 3, combat = 2)
case object GnorriIcon extends UnitClass(Gnorri.name + " Icon", Token, 0)
case object Gnorri extends UnitClass("Gnorri", Monster, 2) with NeutralMonster
```

**Critical parameter notes:**
- `UnitClass` third parameter = **summon cost** in power (NOT combat value)
- `cost` in `NeutralMonsterLoyaltyCard` = **summon cost** in power (same as above)
- `combat` in `NeutralMonsterLoyaltyCard` = **combat dice** rolled in battle
- `doom` is always 2 (hardcoded in the abstract class)

**If the monster has special placement (faction card holding):**
```scala
case object DimensionalShamblerUnit extends UnitClass("Dimensional Shambler", Monster, 2) with NeutralMonster {
    override def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = false
}
case object DimensionalShamblerHold extends UnitClass("Dimensional Shambler (Hold)", Token, 0)
```
Also add a `FactionRegion` in `Game.scala` and full custom summon/deploy action classes. Use the existing Dimensional Shambler as a template.

**Critical — pool-first initialization for special-placement units:**

In `NeutralMonstersAction`, add ALL figures to the reserve pool first, then move one to the holding region:
```scala
// Correct:
lc.quantity.times(DimensionalShamblerUnit).foreach { u =>
    self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num + 1, self.reserve)
}
self.pool(DimensionalShamblerUnit).head.region = ShamblerHold(self)

// Wrong — creates only 1 figure, skips the pool entirely:
self.units :+= new UnitFigure(self, DimensionalShamblerUnit, 1, ShamblerHold(self))
```

In the `SummonAction` handler (e.g. `ShamblerSummonAction`), move an existing pool figure to the holding region — never create a new one with `:+=`:
```scala
// Correct:
self.pool(DimensionalShamblerUnit).head.region = ShamblerHold(self)

// Wrong — creates unbounded extra figures on each summon:
self.units :+= new UnitFigure(self, DimensionalShamblerUnit, ..., ShamblerHold(self))
```

The availability check for the summon action must use `f.pool(DimensionalShamblerUnit).any`, not a total unit count comparison. Total count grows with each incorrect `:+=` creation and the button stops appearing prematurely.

---

### 1. `solo/IGOOs.scala` (IGOOs only)

Define the icon, card, unit, spellbook, and action classes. Pattern from Tulzscha:

```scala
case object TulzschaIcon extends IGOOIcon(...)
case object TulzschaCard extends IGOOLoyaltyCard(TulzschaIcon, Tulzscha, power = 4, combat = 1)
case object Tulzscha extends IGOO(...)
case object CeremonyOfAnnihilation extends IGOOSpellbook(...)
```

Add action case classes for each ability, the trigger logic, the spellbook requirement handler, and the `eliminate` handler. Add the new IGOO to any relevant collections/lists in the file.

**IGOO overlay uses a different function** — see overlay.scala section below.

---

### 2. `solo/Game.scala`

**A. `neutralStrength` function** — add the combat contribution:
```scala
units(Gnorri).num * 2 +    // use actual combat value
```
IGOOs are not added here — their combat is handled separately in GOO battle logic.

**B. Game option declaration:**

For a Neutral Monster:
```scala
case object UseGnorri extends LoyaltyCardGameOption(GnorriCard) with NeutralMonsterOption
```

For an IGOO:
```scala
case object UseTulzscha extends LoyaltyCardGameOption(TulzschaCard) with IGOOOption
```

**C. `GameOptions.all` list** — register the option in the correct position:
```scala
UseGnorri,        // neutral monsters go after UseDimensionalShamblers
UseTulzscha,      // IGOOs go after UseNyogtha
```

**D. Doom Phase ability (if needed)** — for abilities that trigger during the Doom Phase, add inside the `case DoomPhaseAction =>` block, inside the `factions.foreach { f => ... }` loop:
```scala
if (f.loyaltyCards.has(GnorriCard)) {
    val gnorriCount = f.all(Gnorri).num
    if (gnorriCount >= 3) {
        f.doom += 2
        f.log("gained", 2.doom, "from", "Grottos".styled("neutral"), "(3 Gnorri in play)")
    } else if (gnorriCount >= 2) {
        f.doom += 1
        f.log("gained", 1.doom, "from", "Grottos".styled("neutral"), "(2 Gnorri in play)")
    }
}
```

`f.all(UnitClass)` returns all units of that class with `region.inPlay == true` (on the map or in Deep/Slumber regions, but not in pool/reserve).

**E. (IGOOs only) State tracking fields** — add any per-game state needed for the IGOO's abilities:
```scala
var tulzschaFlameTurn : Int = 0
```

---

### 3. `solo/Battle.scala`

Add battle-phase special ability logic in the `PostRoll` section if the unit has a Battle ability. Add after the StarVampire block. Example (Voonith — Vicious):

```scala
// VOONITH - Vicious
sides.foreach { s =>
    val vooniths = s.forces.%(_.uclass == Voonith).num
    if (vooniths > 0) {
        val kills = s.rolls.count(_ == Kill)
        val extra = max(0, vooniths - kills)
        if (extra > 0) {
            s.rolls ++= extra.times(Kill)
            log(Voonith.styled(s), "Vicious added", extra, "Kill".s(extra).styled("kill"))
        }
    }
}
```

Use `.styled("kill")` for "Kill" text to render it red, consistent with the rest of the game. If the unit has no Battle ability, no changes needed here.

#### Post-kill phases with an Ask (e.g. Orifices, Eternal)

If the ability fires **after kills are assigned** (kill phases like `YgolonacOrificesPhase`, `EternalKillPhase`), the phase must present an `Ask` to let the player choose a target or confirm. Follow this pattern exactly:

**Never use `.skip(BattleDoneAction(s))`** in a kill phase. `BattleDoneAction` → `proceed()` re-evaluates the **current** phase. If the dead unit is still in forces (it hasn't been eliminated yet — that happens in `EliminatePhase`), the condition `health == Killed` is still true and the Ask appears again, causing an infinite loop.

**Always use `.skip(BattleProceedAction(NextPhase))`** to make Skip jump directly past the current phase:

```scala
case YgolonacOrificesPhase =>
    sides.foreach { s =>
        val killed = s.forces(Ygolonac).%(_.health == Killed)
        if (killed.any) {
            val targets = s.opponent.forces.%(u => u.health != Killed && ...)
            if (targets.any)
                return Ask(s).each(targets.sortA)(t => YgolonacOrificesAction(s, t).as(t)(...))
                    .skip(BattleProceedAction(EliminatePhase))  // NOT BattleDoneAction
        }
    }
    jump(EliminatePhase)
```

The `EternalKillPhase` uses an alternative approach: it calls `s.remove(Eternal)` **before** the Ask, so when `proceed()` re-enters the phase, the condition is no longer true. Either approach works — mutate state before the Ask, or use `BattleProceedAction` on Skip. The `BattleProceedAction` approach is simpler when the unit can't be removed from forces early.

#### Eliminating a battle unit from outside Battle.scala

If an IGOO action handler (in `IGOOs.scala`) needs to eliminate a unit that is **currently a participant in the active battle**, you must call `game.battle.foreach(_.exempt(targetFigure))` **before** `game.eliminate(targetFigure)`:

```scala
val targetFigure = game.unit(target)
game.battle.foreach(_.exempt(targetFigure))   // removes from forces, adds to exempted list
game.eliminate(targetFigure)
```

`game.eliminate` alone does NOT call `exempt()`. Without the exempt call, the unit stays in the battle's `forces` list with its health reset to `Alive` after elimination. This means it can receive pain assignments in later phases as if it were still alive, which is incorrect.

`Battle.eliminate(u)` (called from within `Battle.scala`) does call `exempt()` automatically. The extra step is only needed when calling from outside the battle context.

---

### 4. `solo/overlay.scala`

**For a Neutral Monster**, add a `loyaltyCard` case:
```scala
case $("Gnorri") => loyaltyCard(GnorriCard.name, GnorriCard.quantity, GnorriCard.cost, GnorriCard.combat,
    "Pay 2 Doom to obtain this Loyalty Card, then place 1 Gnorri at your Controlled Gates.",
    "Grottos", "Doom Phase",
    "During the Doom Phase, if you have 2 Gnorri in play, you earn 1 extra Doom point. If you have 3 Gnorri in play, you earn 2 extra Doom points.")
```

Function signature: `loyaltyCard(name, quantity, summonCost, combat, obtainText, abilityName, abilityPhase, abilityText)`

**For an IGOO**, add a `loyaltyCardIGOO` case with a Boolean pattern variable:
```scala
case $("Tulzscha", spellbook : Boolean) => loyaltyCardIGOO(
    TulzschaCard.name, "" + TulzschaCard.power, "" + TulzschaCard.combat, spellbook,
    "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Tulzscha in the Area containing the Gate.",
    "Undying Flame", "Gather Power Phase", "At the end of the Gather Power Phase: ...",
    "As an Action, each enemy Faction gains 2 Power.",
    "Ceremony of Annihilation", "Doom Phase", "When you perform a Ritual of Annihilation, ...")
```

The `spellbook` Boolean controls whether the spellbook text renders faded (requirement not yet met) or normal. The case pattern `case $("Tulzscha", spellbook : Boolean)` is matched when the spellbook parameter is passed from the loyalty card icon click.

---

### 5. `solo/CthulhuWarsSolo.scala`

This file has the most locations. There are **two mirrored setup blocks** — one for online games and one for local games. Both must receive identical changes. **These blocks look the same but may have different whitespace** — always verify both after editing.

#### A. DrawRect for the unit (one location, shared)

Add after the last similar unit's DrawRect. For larger images, use the faction card vs map conditional:
```scala
case Gnorri => if (region != null) DrawRect("n-gnorri", |(tint), x - 50, y - 90, 100, 100)
                else               DrawRect("n-gnorri", |(tint), x - 30, y - 54, 60, 60)
```
For normal-sized images, a single DrawRect is fine (no if/else needed).

#### B. DrawRect for the icon (one location, shared)

All icons use 50×50:
```scala
case GnorriIcon => DrawRect("gnorri-icon", None, x - 17, y - 55, 50, 50)
```

#### C. (IGOOs only) Spellbook match (one location, shared)

Around line 1098, in the `lc match` block that builds the loyalty card icon spellbook state:
```scala
case TulzschaCard => |(f.upgrades.has(CeremonyOfAnnihilation))
```

#### D. GC Deep sort order (one location, shared)

Add the new unit with the next priority number, and increment Filth's number:
```scala
case DimensionalShamblerUnit => 14
case Gnorri =>                  15   // new
case Filth =>                   16   // was 15
```
Order determines left-to-right rendering sequence on the faction card.

#### E. GC Deep DrawItem cases (one location, shared)

For every unit X that can appear **before** the new unit in the sort sequence, add `(X, NewUnit)`. Then add `(NewUnit, NewUnit)` for the self-pairing. Then add `(NewUnit, Filth)` for the Filth-after case.

Also add `(NewUnit, ...)` outgoing cases in any earlier unit's existing outgoing section if that unit has one (e.g. Tulzscha has explicit `(Tulzscha, X)` cases for all units after it — add `(Tulzscha, Gnorri)` there).

**Spacing values** (the x offset in `DrawItem`) are center-to-center distances. Start by estimating:
```
spacing = prev_effective_half_width + next_effective_half_width
```
Effective half-width is the visual content width (not the bounding box), typically 60–80% of the rendered width. Use an existing unit of similar size as a reference and tune visually.

Key rule: **Cthulhu always uses `6 + last.y`** for the y offset (it's drawn slightly higher on the card).

Pattern:
```scala
case (Cthulhu, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 64 + last.x, 6 + last.y)
case (Abhoth, Gnorri)  => DrawItem(null, f, Gnorri, Alive, $, 52 + last.x, last.y)
// ... one case per preceding unit type, including DeepOne (Alive/Pained variants)
case (Gnorri, Gnorri)  => DrawItem(null, f, Gnorri, Alive, $, 58 + last.x, last.y)
case (Gnorri, Filth)   => DrawItem(null, f, Filth,  Alive, $, 37 + last.x, last.y - 15)
```

DeepOne requires two cases because it can be in two health states:
```scala
case (DeepOne, Gnorri) if last.health == Alive  => DrawItem(null, f, Gnorri, Alive, $, 41 + last.x, last.y)
case (DeepOne, Gnorri) if last.health == Pained => DrawItem(null, f, Gnorri, Alive, $, 41 + last.x, last.y + 31)
```

**Missing any case causes a crash when GC submerges.** The code throws `RuntimeException("GC DEEP missing draw case: ...")` if a combination isn't covered.

#### F. Setup display lines — TWO blocks

In both setup blocks, add a display line inside the `if (setup.options.has(NeutralMonsters))` section. For a neutral monster:
```scala
(setup.options.has(NeutralMonsters))
    .$("Variants" -> ("Use " + GnorriCard.short + " (" + setup.get(UseGnorri).?("yes").|("no").hl + ")")) ++
```

For an IGOO, it goes in the `if (setup.options.has(IGOOs))` section:
```scala
(setup.options.has(IGOOs))
    .$("Variants" -> ("Use " + TulzschaCard.short + " (" + setup.get(UseTulzscha).?("yes").|("no").hl + ")")) ++
```

#### G. Enable-all list — TWO blocks

When NeutralMonsters (or IGOOs) is toggled on, all sub-options are enabled at once. Add the new option to this list in both blocks:
```scala
// Neutral monster:
setup.options ++= $(UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith, UseDimensionalShamblers, UseGnorri)

// IGOO:
setup.options ++= $(UseByatis, UseAbhoth, UseDaoloth, UseNyogtha, UseTulzscha)
```

#### H. Toggle handlers — TWO blocks

Add inside the appropriate `if (setup.options.has(...))` block. **Each item needs its own separate `n -= 1` / `if (n == 0)` block — never merge two options into one block:**
```scala
n -= 1
if (n == 0) {
    setup.toggle(UseGnorri)
    setupQuestions()
}
```

**The display list and the toggle handlers must be in EXACT order and count.** The menu uses sequential index arithmetic: item N in the display list corresponds to the Nth `n -= 1` decrement in the handler. If they're out of sync by even one, clicks will trigger the wrong action.

---

### 6. `solo/index.html`

Add three `<img>` tags in the correct sections:

**Board token** (after the last similar unit's token tag):
```html
<img class="asset" id="n-gnorri" src="webp/images/n-gnorri.webp" />
```

**Icon** (after the last similar unit's icon tag):
```html
<img class="asset" id="gnorri-icon" src="webp/info/n-gnorri.svg" />
```

**Info card** (after the last similar unit's info tag):
```html
<img class="asset" id="info:n-gnorri" src="webp/info/n-gnorri.svg" />
```

The `id` values must match exactly what's used in `DrawRect` and `DrawItem` calls in `CthulhuWarsSolo.scala`.

---

## Build and Test

```bash
# Compile (ALWAYS fastOptJS, never fastLinkJS or clean)
cd ~/cthulhu-wars/solo && sbt fastOptJS

# Restart server
cd ~/cthulhu-wars/online
lsof -ti :999 | xargs kill -9 2>/dev/null
rm -f cwo.lck
sbt "run drop-create-run cwo http://localhost:999/ 999"
```

**Always test in Incognito.** The browser caches JS extremely aggressively. A normal hard refresh is often not enough. Incognito bypasses the cache entirely.

To test submerge: start a local game with GC and the new unit active, submerge GC with the new unit, and verify the faction card renders correctly with no crash.

---

## Common Pitfalls

### Menu clicks trigger the wrong option
The display list and toggle handlers must be in exact sync. Each displayed item corresponds to one `n -= 1` decrement in the handler, in the same order. If a display line is present but the toggle handler is missing (or vice versa), every click below that point will be off by one.

The two setup blocks (online and local) look identical but may have different internal whitespace. Always verify **both** blocks were updated. If a `replace_all` edit claims success but one block is wrong, the second block likely had different whitespace and wasn't matched — edit it separately.

### GC submerge crash
If GC submerges with the new unit and the game crashes, a DrawItem case is missing. The error will say `GC DEEP missing draw case: X -> Y`. Add the missing combination. Common causes:
- Forgot the `(NewUnit, Filth)` case
- Forgot the self-with-self `(NewUnit, NewUnit)` case
- Forgot `(Tulzscha, NewUnit)` in the Tulzscha outgoing section
- Forgot `(Voonith, NewUnit)` or `(DimensionalShamblerUnit, NewUnit)` in their outgoing sections

### Faction card image wrong size or position
If the image appears too large on the faction card, add the `region != null` conditional and specify smaller faction card dimensions. If it appears too low, reduce the y_offset magnitude (less negative = higher). If spacing is off, tune the x values in DrawItem cases.

### Out-of-turn unit placement must call `triggers()` then chain through `CheckSpellbooksAction`

**⚠️ UNTESTED** — this fix is in the source but has not been verified in-game yet.

Any action that places units on the map outside the owner's main action phase (e.g. an out-of-turn deploy prompt) requires two things to award spellbooks immediately:

**1. Call `game.triggers()` before the continuation fires** — `CheckSpellbooksAction` reads `f.unfulfilled`, which is only populated when `triggers()` runs. Normal main actions get this via `AfterAction` → `checkGatesGained` → `triggers()`. Out-of-turn actions must call it explicitly, inside the deploy action handler, before returning `then`:
```scala
// In ShamblerDeployAction, final deploy branch:
else {
    game.triggers()
    then
}
```

**2. Wrap `then` with `CheckSpellbooksAction`** — the out-of-turn prompt continuation must include a spellbook check before resuming the active player's turn:
```scala
// Wrong:
ShamblerDeployPromptAction(f, PreMainAction(e))

// Correct:
ShamblerDeployPromptAction(f, CheckSpellbooksAction(PreMainAction(e)))
```

Both are required. `CheckSpellbooksAction` alone is not enough — it will find nothing if `triggers()` hasn't updated `unfulfilled` yet. And `triggers()` alone is not enough — without `CheckSpellbooksAction` in the chain, the spellbook award has no opportunity to fire until the next player's `AfterAction`.

The card-acquisition deploy path already uses `CheckSpellbooksAction(DoomAction(self))` as its continuation and calls into `AfterAction` naturally, so it is not affected by this pitfall.

### Special-placement units must use the reserve pool — not the holding region — as home

For a unit with `canBeSummoned = false` and a dedicated holding `FactionRegion` (like `ShamblerHold`):

- All figures must be added to `self.reserve` at acquisition time (same as a standard monster). The holding region is just a transit point.
- The summon action must move an existing pool figure to the holding region (`self.pool(uc).head.region = HoldRegion(self)`), not create a new `UnitFigure` with `:+=`.
- The availability check must be `f.pool(uc).any`, not a total unit count comparison.

If you create new figures on each summon, killed units accumulate in the pool (correctly returned by `eliminate`), new figures pile up at the holding region, and the summon button eventually stops appearing because the total figure count exceeds the card's quantity. The symptom — "killed unit returns to faction card" — is actually the pool unit being rendered on the faction card's pool area.

### Action menu items must use `uc.styled(self)` for unit names, not `uc.name.styled("neutral")`

Any `BaseFactionAction` or `OptionFactionAction` that displays a unit name must use `uc.styled(self)` to get the faction's color. Using `uc.name.styled("neutral")` hardcodes the neutral/gray color instead of the owning faction's color.

To display a power cost in a `BaseFactionAction` header, use the `implicit g =>` lambda form and call `g.forNPowerWithTax(r, self, cost)`. This handles tax automatically and formats power in white. When the action targets the faction card (not a map region), pass `self.reserve` as the region — tax will be 0 and the correct cost appears:

```scala
// Wrong — hardcoded neutral color, no power cost:
case class ShamblerSummonAction(self : Faction) extends BaseFactionAction(
    "Summon " + DimensionalShamblerUnit.name.styled("neutral"),
    "to Faction Card".styled("neutral"))

// Correct — faction color, power cost in white, faction-colored destination:
case class ShamblerSummonAction(self : Faction) extends BaseFactionAction(
    implicit g => "Summon " + DimensionalShamblerUnit.styled(self) + g.forNPowerWithTax(self.reserve, self, self.summonCost(DimensionalShamblerUnit, self.reserve)),
    "to " + "Faction Card".styled(self))
```

The same applies to `OptionFactionAction` (top-level menu entries): use `uc.styled(self)` directly in the string argument — no lambda needed since no power cost is shown at that level.

### summon cost vs. combat value confusion
- `UnitClass("Name", Monster, X)` — X is the **summon cost** in power
- `NeutralMonsterLoyaltyCard(..., cost = X, ...)` — X is also the **summon cost** in power
- `NeutralMonsterLoyaltyCard(..., combat = X)` — X is the **combat dice**

These are easy to swap. Double-check all three. A real example: Gnorri was defined as `UnitClass("Gnorri", Monster, 2)` (combat value) while `GnorriCard` correctly had `cost = 3`. The unit showed a 2-power summon cost in-game despite the card saying 3.

### Infinite loop in battle phase Ask with Skip

If a kill-phase `Ask` has `.skip(BattleDoneAction(s))`, the Skip button causes an infinite loop: `BattleDoneAction` → `proceed()` → current phase re-evaluated → dead unit is still in forces with `health == Killed` → Ask appears again. **Always use `.skip(BattleProceedAction(EliminatePhase))` (or the next appropriate phase) instead.** See the Battle.scala section for the full pattern.

### Eliminating a battle participant from an IGOO action handler

If your `ForcedAction` handler in `IGOOs.scala` eliminates a unit that's currently in battle, call `game.battle.foreach(_.exempt(targetFigure))` **before** `game.eliminate(targetFigure)`. Without it, the unit remains in the battle's `forces` list with health reset to `Alive` after elimination, and can incorrectly receive pain assignments in later phases. `Battle.eliminate()` does this automatically when called from inside `Battle.scala`; the extra step is required only when calling from outside.

### Online game crash: "Unknown symbol: XCard"

When running as an **online** game, the server serializes and deserializes game state between players. Every loyalty card must be registered in `Serialize.scala` or the deserializer throws `Unknown symbol: XCard` and crashes.

**After adding any new loyalty card, add it to the `loyaltyCards` list in `solo/Serialize.scala`:**

```scala
val loyaltyCards = $(..., TulzschaCard, GnorriCard, YgolonacCard)
```

This is the only place requiring manual registration. Unit classes (e.g. `Gnorri`, `Ygolonac`), spellbooks (e.g. `TheRevelations`), and game options (e.g. `UseGnorri`) are all found automatically by `parseSymbol` via reflection — only loyalty cards need the explicit list entry.

This crash **will not appear in solo/local games** — only online. Always test an online game after adding a new unit to catch this early.

### Online undo crash: "rollback-v2" 404

The undo button in online games calls `POST /rollback-v2/{role}/{index}` on the server. This endpoint was not in the original server — if it's missing, undo fails with a 404 and shows "undo failed, reloading".

**The fix lives in `online/CthulhuWarsOnline.scala`**, not in the solo JS. Add the route after the `write` route:

```scala
(post & path("rollback-v2" / Segment / IntNumber)) { (role, index) =>
    q(roles.filter(_.secret === role).map(_.gameId).result.head.flatMap { id =>
        logs.filter(_.gameId === id).filter(_.index >= index).delete
    })
    complete(StatusCodes.Accepted)
}
```

This looks up the game by role secret (same pattern as `read`) and deletes all log entries at index >= n, effectively rolling the game back to that point. After adding this route, restart the online server — no JS rebuild needed since this is a server-only change.

**How the rollback index arithmetic works:**

- The 3 initial DB entries (game setup data) occupy indices 0–2
- Each recorded action is stored at `DB index = actions.num + 3` (where `actions.num` is the 0-based count before that action is applied)
- When the player clicks undo on a log entry shown after N actions, the client sends `N + 3` as the rollback index
- `filter(_.index >= N+3).delete` removes actions N, N+1, ... keeping exactly the N actions shown at the undo point

**What undo does and does not do to `acted`:**

- Forced action chains (`SummonedAction → EndAction → AfterAction`) are **not** stored as independent DB entries per se — they re-derive from game logic when the recorded root action replays. So rolling back past a `SummonAction` restores power and `acted=false` (correct), but the unit is also gone from the board. The player can summon again as a genuine redo.
- Undo **cannot** produce a state where a unit is visible on the board AND `acted=false` for that summon. EndAction always re-runs as part of the chain.
- Undo **can** produce `acted=false` mid-turn legitimately: if a player took action A (recorded) but had not yet pressed End Turn when someone undoes to just after action A. This is correct undo behavior.

### Action handlers must guard their own power check — don't rely on the menu builder alone

The menu builder (`game.summons(f)`, `game.awakens(f)`, etc.) filters options by `affords(cost)(r)` at display time. In online games, there is an async gap between when the player clicks an option and when the action actually executes via DB read/replay. In that window, game state can change (undo fired, another player acted, race condition). If the handler doesn't re-check affordability, it can deduct power from an already-zero balance and produce negative power.

**Always add an `affords` guard inside the handler:**
```scala
// SummonAction:
case SummonAction(self, uc, r) =>
    if (self.pool(uc).none || self.affords(self.summonCost(uc, r))(r).not)
        EndAction(self)
    else {
        self.power -= self.summonCost(uc, r)
        ...
    }

// AwakenAction:
case AwakenAction(self, uc, r, cost) =>
    if (self.pool(uc).none || self.affords(cost)(r).not)
        EndAction(self)
    else {
        self.power -= cost
        ...
    }

// IndependentGOOAction (if adding a new IGOO):
case IndependentGOOAction(self, lc, r, _) =>
    if (game.loyaltyCards.has(lc).not || self.affords(lc.power)(r).not)
        EndAction(self)
    else {
        self.power -= lc.power
        ...
    }
```

The symptom of a missing guard is the log showing "somehow ran into negative power" — which is logged by `AfterAction` when `self.power < -1` after the action chain completes. The `AfterAction` silently corrects power back to 0 but logs the anomaly.

### Command settings groups must be gated by the same condition as their deploy prompt

When an out-of-turn unit ability has plan-based command settings (the Configure-when-to-prompt menu), those settings must be hidden whenever the underlying action itself cannot currently be taken. If not gated, the settings menu appears in the out-of-turn Commands panel even when the action is impossible — confusing the player.

The Dimensional Shambler had this bug: `ShamblerPlan` settings remained visible even when no Shambler was in `ShamblerHold` (all units were on the map or in the reserve pool). The deploy prompt itself was correctly hidden by `f.at(ShamblerHold(f), DimensionalShamblerUnit).any`, but the command settings were not.

**Two places in `Game.scala` must be updated together:**

`extraActions` — controls the info display and the `CommandsMainAction` button in the out-of-turn section:
```scala
{ val vp = f.plans.%(p => p.is[ShamblerPlan].not || f.at(ShamblerHold(f), DimensionalShamblerUnit).any)
vp.%(f.commands.has)./(p => Info(p.info)(p.group)) ++
vp.any.$(CommandsMainAction(f)) }
```

`CommandsMainAction(f)` handler — controls what appears inside the menu when opened:
```scala
val visiblePlans = f.plans.%(p => p.is[ShamblerPlan].not || f.at(ShamblerHold(f), DimensionalShamblerUnit).any)
Ask(f).each(visiblePlans) { p =>
```

**When adding a new unit with plan-based command settings:** add a parallel filter condition in both locations using the same predicate that gates the unit's out-of-turn prompt in `PreActionPromptsAction`. Introduce a new `sealed abstract class MyUnitPlan` for the unit's plans (analogous to `ShamblerPlan`, `DevolvePlan`, etc.) and use `.is[MyUnitPlan]` in the filter.

### IGOO `Use*` option missing `with IGOOOption` — duplicate loyalty cards in game

Every `Use*` option for an IGOO must extend `IGOOOption`. Missing this trait causes two bugs:

1. **`IGOOsExpansion` not loaded when only that IGOO is enabled.** The expansion is gated on `options.of[IGOOOption].any.$(IGOOsExpansion)`. Without the trait, the expansion is skipped and none of the IGOO's action handlers fire.

2. **Duplicate loyalty cards when the IGOOs group is toggled on/off multiple times.** The "Use IGOOs" group toggle uses `++=` (not deduplicated) to add all IGOO options, and uses `notOf[IGOOOption]` to remove them. If the `Use*` option is missing `IGOOOption`, it survives the cleanup but gets appended again on the next enable. After `n` enable/disable cycles, the card appears `n` times in `game.loyaltyCards`, and `independents()` generates `n` awaken options — the player can awaken the IGOO `n` times in one turn.

**Fix:** always add `with IGOOOption` to every IGOO's `Use*` option:
```scala
// Correct:
case object UseTulzscha extends LoyaltyCardGameOption(TulzschaCard) with IGOOOption
case object UseYgolonac extends LoyaltyCardGameOption(YgolonacCard) with IGOOOption
```

This was the root cause of Tulzscha being awakened 4 times in a single action phase in a multiplayer game.

### IGOO spellbook actions that modify game state must NOT be `Soft`

Any action class that directly modifies game state (sets `self.acted = true`, grants power, satisfies spellbook requirements, etc.) **must not** have `with Soft`. `Soft` means the action is not written to the online DB. Other clients will never see it and will have a desync — their game state diverges from the acting player's state.

`Soft` is appropriate only for pure navigation actions (like opening a sub-menu or expanding options) that don't modify game state. If an action does something and returns a game-changing result, it must be recorded (no `Soft`).

Example: `CeremonyOfAnnihilationChoiceAction` was `with Soft` by mistake. When Opener clicked Ceremony, their local state had `Opener.acted = true` and gained power — but other clients replayed the resulting `DoomAction(Opener)` without the ceremony having happened. This desync caused `DoomDoneAction` to fire out-of-sync, skipping the next player's doom turn.

**Fix:** remove `with Soft` from any IGOO spellbook action that modifies state:
```scala
// Wrong — not recorded, causes desync:
case class CeremonyOfAnnihilationChoiceAction(self : Faction) extends OptionFactionAction(...)
    with DoomQuestion with Soft

// Correct — recorded, both clients apply it before the follow-up DoomAction:
case class CeremonyOfAnnihilationChoiceAction(self : Faction) extends OptionFactionAction(...)
    with DoomQuestion
```

This is distinct from `DoomQuestion` (just a UI marker) and from `OptionFactionAction` (just controls rendering as an expandable option). Neither of those affects recording behavior. Only `Soft`, `Cancel`, `Void`, or `More` traits affect whether an action is recorded.

### Out-of-turn recorded actions in `PreActionPromptsAction` are susceptible to 409 Conflict desync

`PreActionPromptsAction` is the window where out-of-turn prompts (Shambler deploy, Tulzscha give-power, etc.) fire before the active player's action. In async online mode, this is precisely the window where the active player is most likely to simultaneously write an action to the DB. If the out-of-turn action is recorded (not Soft), both clients may write to the same DB position — causing a 409 Conflict.

The 409 handler retries by reading the DB's actual entry and replaying it on the already-mutated local game object `g`, producing a corrupted game state. This corruption can silently break any subsequent game logic — including `preroll` tag-setting in `Battle.scala`, which means spellbooks that depend on battle tags (like MFO) may never fire.

**Observed symptom (Bug 17):** Opener has Shambler on the faction card. `ShamblerDeployAction` writes to the DB during `PreActionPromptsAction` at the same time the enemy writes. The resulting 409 corruption propagates into the battle and MFO is skipped. Undo + replay fixes it because replay uses the DB as the source of truth, bypassing the corrupted local state.

**This is the same architecture as Bug 13** (double turn in action phase). There is no clean code fix — the retry handler is the fundamental issue.

**Design implication for new units:** If a new unit's out-of-turn ability produces a recorded action during `PreActionPromptsAction`, it inherits this same susceptibility. The only currently-known mitigation is the undo/redo workaround. Do not add `Soft` to the action to "avoid" the bug — that would cause a different desync where only one client sees the deploy (Bug 12 pattern). Document the known susceptibility if it cannot be avoided.

### `sbt fastLinkJS` or `sbt clean`
- **Never use `sbt fastLinkJS`** — it produces a directory-format output that `index.html` can't reference. The build will report success, but the browser silently continues serving the old `.js` file. Code changes appear to have no effect even in Incognito. Always use `sbt fastOptJS`.
- If you accidentally ran `fastLinkJS`, just run `sbt fastOptJS` afterward — no clean needed.
- **Never use `sbt clean`** unless absolutely necessary — it deletes the compiled `.js` file

### Browser shows old behavior
Always use Incognito. If even Incognito shows old behavior, confirm the server was restarted after the build and is serving from the right directory.

---

## Neutral Monster vs. IGOO: Key Differences

| Aspect | Neutral Monster | IGOO |
|--------|----------------|------|
| Scala file | `NeutralMonsters.scala` | `IGOOs.scala` |
| Base class | `NeutralMonsterLoyaltyCard` | `IGOOLoyaltyCard` |
| Doom cost | Always 2 (hardcoded) | Variable |
| Awaken requirements | None (standard summon) | Specific conditions + power cost |
| Spellbook | No | Yes (with requirement) |
| Overlay function | `loyaltyCard(...)` | `loyaltyCardIGOO(...)` |
| Overlay case pattern | `case $("Name")` | `case $("Name", spellbook : Boolean)` |
| Game option trait | `with NeutralMonsterOption` | No trait (plain `LoyaltyCardGameOption`) |
| Setup section | Under `NeutralMonsters` toggle | Under `IGOOs` toggle |
| `Game.scala` option | After `UseDimensionalShamblers` | After `UseNyogtha` |
| `neutralStrength` entry | Yes | No |
| Battle.scala | If battle ability | Usually not needed |

---

## Reference: Faction Card Rendering Sizes

Existing units on the faction card (GC Deep display):

| Unit | Faction Card Size | Map Size |
|------|-----------------|----------|
| Ghast | 35×59 | same |
| Gug | 73×90 | same |
| Shantak | 79×100 | same |
| StarVampire | 70×85 | same |
| Voonith | 45×57 | 70×85 |
| DimensionalShambler | 58×70 | 70×85 |
| Tulzscha | 100×100 | 137×137 |
| Gnorri | 60×60 | 100×100 |

Units where faction card = map size have no `region != null` conditional. Units with different sizes do.
