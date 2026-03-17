# Adding a New Neutral Monster to Cthulhu Wars Online
*Updated after implementing Voonith and Dimensional Shambler*

---

## Before You Start

Define your monster's stats:
- **Name**: e.g. "Dimensional Shambler"
- **Scala object name**: e.g. `DimensionalShamblerUnit` (use a suffix if "Shambler" alone is ambiguous)
- **Quantity**: how many figures (e.g. 3)
- **Loyalty Card Doom Cost**: doom paid to acquire the card (e.g. 2)
- **Summon Cost**: power paid per figure to summon (e.g. 2) — this is the `UnitClass` third parameter
- **Combat Value**: dice rolled in battle (e.g. 2) — goes in `NeutralMonsterLoyaltyCard`
- **Special Ability**: name, phase, description
- **Placement type**: Standard (place at gate) or Special (place on faction card, like Dimensional Shambler)

---

## Practical Notes Before Coding

- **Always use Python scripts with `content.replace()`** for file edits, never sed. Scala source files contain characters that break zsh/sed escaping.
- **Always check for tab vs space issues** before replacements fail. If a replacement returns 0 occurrences, inspect the exact bytes with `python3` reading `repr(line)`.
- **Test in Incognito mode**. The browser caches JS very aggressively. A regular hard refresh (Cmd+Shift+R) is often not enough. Incognito mode bypasses the cache entirely and is the most reliable way to test changes.
- **After any failed replacement**, always re-read the file with `sed -n` or `python3 repr` to see the exact current content before trying again.

---

## Files to Change

### 1. `solo/NeutralMonsters.scala`

Add three or four objects. Follow this pattern exactly:

```scala
case object VoonithCard extends NeutralMonsterLoyaltyCard(VoonithIcon, Voonith, cost = 2, quantity = 2, combat = 1)
case object VoonithIcon extends UnitClass(Voonith.name + " Icon", Token, 0)
case object Voonith extends UnitClass("Voonith", Monster, 3) with NeutralMonster
```

**Critical parameter notes:**
- Third parameter of `UnitClass` = **summon cost** in power (NOT combat value)
- `cost` in `NeutralMonsterLoyaltyCard` = **doom cost** to acquire the loyalty card
- `combat` in `NeutralMonsterLoyaltyCard` = combat dice rolled in battle

**If the monster has special placement (e.g. goes to faction card instead of map):**

Override `canBeSummoned` to prevent it appearing in the normal summon menu:
```scala
case object DimensionalShamblerUnit extends UnitClass("Dimensional Shambler", Monster, 2) with NeutralMonster {
    override def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = false
}
```

Add a dedicated `FactionRegion` to hold figures on the faction card (in `Game.scala`, see step 2).

Add custom summon and deploy action classes and handlers — see the Dimensional Shambler implementation as a template.

---

### 2. `solo/Game.scala`

Four additions:

**A. `neutralStrength` function** — add combat strength contribution (after StarVampire line):
```scala
units(Voonith).num * 1 +   // use the monster's combat value
```

**B. `UseVoonith` case object** — declare the game option (after UseStarVampire):
```scala
case object UseVoonith extends LoyaltyCardGameOption(VoonithCard) with NeutralMonsterOption
```

**C. `GameOptions.all` list** — register the option (after UseVoonith or UseStarVampire):
```scala
UseVoonith,
```
**Important**: Check the exact whitespace/tabs of the surrounding lines before replacing — this list often has mixed indentation from previous edits.

**D. (Special placement only) Add a FactionRegion** for monsters that live on the faction card:
```scala
case class ShamblerHold(faction : Faction) extends FactionRegion {
    val glyph = Pool
    val id = "ShamblerHold"
    val name = faction.name + " Shambler Hold"
}
```
Add after the other FactionRegion definitions (Deep, Slumber, etc.).

---

### 3. `solo/Battle.scala`

Add the special ability logic in the `PostRoll` phase. Add after the StarVampire ability block.

Example (Vicious):
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

Always use `.styled("kill")` to make "Kill" appear red, consistent with the rest of the game.

---

### 4. `solo/overlay.scala`

Add the info card popup in the `info` function's pattern match. Add before the Star Vampire case:

```scala
case $("Voonith") => loyaltyCard(VoonithCard.name, VoonithCard.quantity, VoonithCard.cost, VoonithCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Voonith at your controlled Gate.", "Vicious", "Battle", "After rolling dice in Battle...")
```

Function signature:
```scala
def loyaltyCard(name, quantity, cost, combat, obtainText, abilityName, abilityPhase, abilityText)
```

---

### 5. `solo/CthulhuWarsSolo.scala`

This file has the most changes — **six locations** across **two mirrored blocks** (online setup UI and solo setup UI). Each block needs identical changes.

**A. Display line** — show the monster in the setup menu. Add after the Voonith display line, in both blocks:
```scala
(setup.options.has(NeutralMonsters))
    .$("Variants" -> ("Use " + VoonithCard.short + " (" + setup.get(UseVoonith).?("yes").|("no").hl + ")")) ++
```

**B. Enable-all list** — add to the list when NeutralMonsters is toggled on (in both blocks):
```scala
setup.options ++= $(UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith)
```

**C. Toggle handler** — add a new `n -= 1` / `if (n == 0)` block after Voonith's block (in both blocks). **Each item needs its own separate block** — never merge two toggles into one `if (n == 0)`:
```scala
n -= 1
if (n == 0) {
    setup.toggle(UseVoonith)
    setupQuestions()
}
```
**Common failure mode**: if you paste the block with wrong indentation (tabs instead of spaces or vice versa), the replacement will silently fail. Always check with `python3 repr()` if a replacement returns 0 occurrences.

**D. Board rendering — DrawRect** — add how the unit is drawn on the map. Add after the Voonith DrawRect case:
```scala
case Voonith => DrawRect("n-voonith", |(tint), x - 35, y - 75, 70, 85)
```
Adjust pixel offsets to match your actual image dimensions.

**E. GC Deep sort order** — add to the sort order for units submerged by Great Cthulhu. Add after Voonith, and increment Filth's number:
```scala
case Voonith =>     13
case Filth =>       14
```

**F. GC Deep DrawItem cases** — add positioning cases for the unit appearing in GC's Deep alongside other units. Add a full block before the Filth cases. Use StarVampire's offsets as a starting point:
```scala
case (Cthulhu, Voonith) => DrawItem(null, f, Voonith, Alive, $, 79 + last.x, 6 + last.y)
case (Abhoth, Voonith) => DrawItem(null, f, Voonith, Alive, $, 63 + last.x, last.y)
// ... (one case per preceding unit type)
case (Voonith, Voonith) => DrawItem(null, f, Voonith, Alive, $, 65 + last.x, last.y)
```
Also add Voonith to the Filth cases:
```scala
case (Voonith, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
```

**G. (Special placement only) Faction card rendering** — for monsters that live on the faction card (like Dimensional Shambler), render them directly using `dd()` after `draws.reverse.foreach(dd)`:
```scala
val iconsWidth = f.loyaltyCards.num * 30 + 60
f.at(ShamblerHold(f)).reverse.zipWithIndex.foreach { case (u, i) =>
    val x = w - iconsWidth - 40 - i * 40
    val y = h - 85 - 12
    dd(DrawItem(null, f, DimensionalShamblerUnit, Alive, $, x + 35, y + 75).rect)
}
```
**Key lessons learned:**
- Do NOT add special-placement monsters to the `deep`/`captured` layout system — render them separately after `draws.reverse.foreach(dd)`
- Position them on the **right side** of the faction card, just left of the loyalty card icons
- `iconsWidth` must account for the number of loyalty cards the faction holds
- The faction card canvas is 450 units tall; `h - 85 - 12` positions the bottom of an 85px-tall image 12 units from the bottom
- Use `DrawItem(...).rect` (not `DrawRect(...)` directly) to get faction tinting automatically
- The `dd` function draws at the image's natural pixel size — make sure your webp image is the right pixel dimensions (see Image Files section below)

---

### 6. `solo/index.html`

Add three `<img>` tags. Add after the Voonith entries in each section:

**Board token** (~line 822):
```html
<img class="asset" id="n-voonith" src="webp/images/n-voonith.webp" />
```

**Icon** (~line 834):
```html
<img class="asset" id="voonith-icon" src="webp/info/n-voonith.svg" />
```

**Info card** (~line 933):
```html
<img class="asset" id="info:n-voonith" src="webp/info/n-voonith.svg" />
```

---

### 7. Image Files

Two files required in `solo/webp/`:

- `solo/webp/images/n-voonith.webp` — board token (drawn on map and faction card)
- `solo/webp/info/n-voonith.svg` — info card (shown in loyalty card popup and as faction card icon)

Naming: `n-` + monster name in lowercase with spaces replaced by hyphens.

**Critical: image pixel dimensions must match the DrawRect dimensions** exactly. The faction card renders images at their natural pixel size (no scaling). If the image is too large, it will appear enormous on the faction card.

- Check dimensions: `sips -g pixelWidth -g pixelHeight path/to/image.webp`
- Resize with ImageMagick (install via `brew install imagemagick` if needed):
  `convert input.webp -resize 70x85! output.webp`
- `sips` cannot write webp format — use ImageMagick instead

Typical dimensions used by existing monsters:
- Small monster (Ghast): 35×59px
- Medium monster (Star Vampire, Voonith, Dimensional Shambler): 70×85px
- Large monster (Gug): 73×90px

For development placeholders, download from the live server:
```bash
curl -o solo/webp/images/n-voonith.webp https://cwo.im/hrf/webp/images/n-star-vampire.webp
curl -o solo/webp/info/n-voonith.svg https://cwo.im/hrf/webp/info/n-star-vampire.svg
```

---

## Build & Test

```bash
# Compile
cd ~/cthulhu-wars/solo && sbt fastOptJS

# Kill any running server and restart
cd ~/cthulhu-wars/online
lsof -ti :999 | xargs kill -9
rm -f cwo.lck
sbt "run drop-create-run cwo http://localhost:999/ 999"
```

**Always test in Incognito mode.** The browser caches JS very aggressively. A regular hard refresh (Cmd+Shift+R) is often not enough and will show you stale behavior. Incognito mode bypasses the cache entirely.

---

## Common Pitfalls

- **Game hangs on Start**: Usually means a display item was added without a matching `n -= 1` toggle block, or two toggles were merged into one `if (n == 0)` block. Each clickable menu item needs its own separate block.
- **Summon cost wrong**: The third parameter of `UnitClass("Name", Monster, X)` is the summon cost in power. Don't confuse it with combat value.
- **Replacement returns 0 occurrences**: Almost always a whitespace mismatch (tabs vs spaces). Use `python3` to print `repr(line)` for lines around the target and copy the exact characters.
- **Submerge crash with GC**: The GC Deep region has a hardcoded sort order and DrawItem match for every unit type. Any new neutral monster must be added to both.
- **"Kill" text not red in log**: Use `.styled("kill")` on the string.
- **Faction card image too large**: The `dd()` function draws at natural pixel size with no scaling. Resize the webp to match DrawRect dimensions using ImageMagick.
- **Faction card image position wrong**: Don't use the `deep`/`captured` layout system for special-placement monsters. Render directly with `dd()` after `draws.reverse.foreach(dd)`, positioned from the right side of the card.
- **Console logs not appearing**: Scala.js `dom.console.log` outputs to the browser console but will be hidden by cache. Test in Incognito. Also check the compiled JS directly with `grep` to confirm the log statement compiled correctly.
- **Use Python scripts, not sed**: The Scala source files contain characters that break zsh/sed escaping.
