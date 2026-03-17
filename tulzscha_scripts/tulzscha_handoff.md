# Tulzscha IGOO Implementation — Claude Code Handoff

## Context

This is a Scala/sbt project at `~/cthulhu-wars/solo/` implementing Tulzscha as an Independent Great Old One (IGOO) for Cthulhu Wars Online. The project compiles with `cd ~/cthulhu-wars/solo && sbt fastOptJS`.

All file edits must be done with Python scripts using `content.replace()` — never sed, because the Scala files contain characters that break shell escaping.

---

## What Has Already Been Done (DO NOT REDO)

All scripts are in `~/cthulhu-wars/tulzscha_scripts/`. The following have been successfully applied:

### solo/IGOOs.scala — COMPLETE
- `TulzschaCard`, `TulzschaIcon`, `Tulzscha` UnitClass added
- `CeremonyOfAnnihilation` NeutralSpellbook added
- `CeremonyOfAnnihilationChoiceAction` action class added
- `TulzschaGivePowerMainAction`, `TulzschaGivePowerAction` action classes added
- `checkTulzschaUndyingFlame()` trigger added (calls `game.isAfterGatherPower` — **may need field name fix at compile**)
- Tulzscha `eliminate` handler added
- Tulzscha `awaken` case in `IndependentGOOAction` added (grants `CeremonyOfAnnihilation`)
- Ceremony of Annihilation ritual intercept added in `perform()`:
  - Intercepts `RitualAction(self, cost, k)` when faction has Tulzscha + CeremonyOfAnnihilation
  - Offers choice between normal ritual and Ceremony
  - Ceremony handler: earns `game.ritualCost` power, advances `game.ritualMarker`, calls `game.showROAT()`, satisfies `PerformRitual`
- `TulzschaGivePowerAction` perform handler added (gives each enemy 2 power, grants CeremonyOfAnnihilation if not already held)

### solo/Game.scala — COMPLETE
- `UseTulzscha extends LoyaltyCardGameOption(TulzschaCard)` added after `UseNyogtha`
- `UseTulzscha` registered in `GameOptions.all`

### solo/overlay.scala — COMPLETE
- Tulzscha case added to info pattern match:
  ```
  case $("Tulzscha", spellbook : Boolean) => loyaltyCardIGOO(TulzschaCard.name, "" + TulzschaCard.power, "" + TulzschaCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Tulzscha in the Area containing the Gate.", "Undying Flame", "Gather Power Phase", "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom than you; gain 1 Elder Sign if any Faction has more Elder Signs than you; gain 1 Power if any Faction has more Power than you.", "As an Action, each enemy Faction gains 2 Power.", "Ceremony of Annihilation", "Doom Phase", "When you perform a Ritual of Annihilation, you may choose to pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs.")
  ```

### solo/CthulhuWarsSolo.scala — MOSTLY COMPLETE
- `UseTulzscha` added to enable-all list (2 occurrences) ✓
- `UseTulzscha` toggle handler added (2 occurrences) ✓
- `Tulzscha DrawRect` added after Nyogtha DrawRect ✓
- GC Deep sort order: Tulzscha added with value 4 ✓
- GC Deep DrawItem cases: `(X, Tulzscha)` cases fixed (were incorrectly pointing to Nyogtha) ✓
- `(Tulzscha, X)` follower DrawItem cases added ✓
- **STILL BROKEN**: Setup display line — see "Remaining Work" below

### solo/index.html — COMPLETE
- `<img class="asset" id="n-tulzscha" src="webp/images/n-tulzscha.webp" />` added
- `<img class="asset" id="tulzscha-icon" src="webp/info/n-tulzscha.svg" />` added
- `<img class="asset" id="info:n-tulzscha" src="webp/info/n-tulzscha.svg" />` added

---

## Remaining Work

### 1. Fix broken setup display line in CthulhuWarsSolo.scala (PRIORITY)

A previous script attempted to insert the Tulzscha setup display line but placed it mid-method-chain, causing a compile error. The file has already been partially cleaned but the correct insertion has not been made yet.

**The compile error was:**
```
[error] CthulhuWarsSolo.scala:2038:25: ')' expected but '.' found.
[error]     .$("Variants" -> ("Use " + TulzschaCard.short + ...)) ++
```

**What needs to happen:**

Run this diagnostic script first to see the exact context:

```python
#!/usr/bin/env python3
# Save as /tmp/diag_display.py and run: python3 /tmp/diag_display.py
import os, re

def find_sdir():
    for start in [os.path.expanduser("~/cthulhu-wars")]:
        for dp, dns, fns in os.walk(start):
            if "IGOOs.scala" in fns: return dp
            dns[:] = [d for d in dns if not d.startswith('.') and d not in ('node_modules','target')]
    return None

sdir = find_sdir()
path = os.path.join(sdir, "CthulhuWarsSolo.scala")
with open(path) as f:
    lines = f.readlines()

print(f"Total lines: {len(lines)}")

# Show all lines mentioning UseNyogtha with line numbers and context
for i, line in enumerate(lines):
    if 'UseNyogtha' in line and 'NyogthaCard.short' in line:
        start = max(0, i - 10)
        end = min(len(lines), i + 12)
        print(f"\n=== UseNyogtha display at line {i+1} ===")
        for j in range(start, end):
            marker = ">>>" if j == i else "   "
            print(f"{marker} {j+1:4d}: {repr(lines[j])}")

# Also check if bad Tulzscha display line still present
for i, line in enumerate(lines):
    if 'TulzschaCard.short' in line:
        print(f"\nFound TulzschaCard.short at line {i+1}: {repr(line)}")
```

**After seeing the context**, the fix script should:
1. Remove any remaining bad `TulzschaCard.short` display lines
2. Find the correct position in the method chain (the `.$(...)  ++` lines are part of a chain — Tulzscha needs to go AFTER Nyogtha's line, on its own `.$()  ++` line, continuing the chain)
3. Insert in both occurrences (online setup block and solo setup block)

The existing Nyogtha display line looks like:
```scala
.$("Variants" -> ("Use " + NyogthaCard.short + " (" + setup.get(UseNyogtha).?("yes").|("no").hl + ")")) ++
```

The Tulzscha line to add directly after it (same indentation, same `++` continuation):
```scala
.$("Variants" -> ("Use " + TulzschaCard.short + " (" + setup.get(UseTulzscha).?("yes").|("no").hl + ")")) ++
```

The problem from the previous attempt: the insert was placed after the Nyogtha line but the Nyogtha line already ends with `++` which chains to the next line. Adding a new `.$()  ++` line right after should be syntactically correct IF the next line after the Nyogtha line is another `.$()` or similar chain continuation — but if the Nyogtha line is the LAST in the chain before a closing paren or different expression, the `++` on the Tulzscha line would be dangling.

**Look at the context carefully**: if the Nyogtha line is followed by e.g. `.$("Done" -> ...)` or a closing paren, the Tulzscha line needs to go BEFORE the last item, not after. The chain probably ends with something like:
```scala
.$("Done" -> "Start game".styled("power")),
```
...which means Tulzscha must go BEFORE that final line, not after the Nyogtha line.

### 2. Verify Undying Flame trigger field name

In `IGOOs.scala`, `checkTulzschaUndyingFlame()` uses `game.isAfterGatherPower`. This field name is guessed — it may not exist. After fixing the display line and getting a clean compile, if there's an error about `isAfterGatherPower`, search Game.scala for how the Gather Power phase end is detected and use that pattern instead.

The Undying Flame fires at the END of Gather Power phase. Look in Game.scala for how other phase-end triggers work (e.g. search for `GatherPower` or `AfterGather` or phase transition logic).

If `isAfterGatherPower` doesn't compile, the simplest fix is to trigger it differently — the `triggers()` function in IGOOsExpansion is called each turn, so add a phase check like:
```scala
if (game.phase == GatherPower && game.phaseEnding) { ... }
```
...using whatever the actual phase enum and phase-end flag are called in Game.scala.

### 3. Verify Ceremony of Annihilation intercept compiles

The intercept in IGOOs.scala perform() uses:
- `game.ritualCost` — confirmed as `def ritualCost = min(10, ritualTrack(ritualMarker))` in Game.scala ✓
- `game.ritualMarker` — confirmed as `var ritualMarker = 0` in Game.scala ✓  
- `game.showROAT()` — confirmed called in the RitualAction body in Game.scala ✓
- `game.ritualTrack` — confirmed used in Game.scala ✓

These should all compile. If `showROAT` is private, use a workaround or just remove that call (it's cosmetic — updates the ritual track display).

---

## Key Facts About the Codebase

- **Repo**: `~/cthulhu-wars/`
- **Scala source**: `~/cthulhu-wars/solo/`
- **Compile**: `cd ~/cthulhu-wars/solo && sbt fastOptJS`
- **Server restart**:
  ```bash
  cd ~/cthulhu-wars/online
  lsof -ti :999 | xargs kill -9
  rm -f cwo.lck
  sbt "run drop-create-run cwo http://localhost:999/ 999"
  ```
- **Always test in Incognito** (browser caches JS aggressively)
- **Always use Python scripts** for file edits — never sed
- **Images already in place**: `solo/webp/images/n-tulzscha.webp` and `solo/webp/info/n-tulzscha.svg`

## Tulzscha Rules Summary

**Awaken cost**: 4 Power. Requirements: your controlled Gate is in an Area with your Great Old One. Place Tulzscha in the Area containing the Gate.

**Combat**: 1

**Undying Flame** (Gather Power Phase — end of phase): Gain 1 Doom if any faction has more Doom. Gain 1 Elder Sign if any faction has more Elder Signs. Gain 1 Power if any faction has more Power.

**Spellbook Requirement**: As an Action, each enemy faction gains 2 Power. (This is what `TulzschaGivePowerAction` does — it grants CeremonyOfAnnihilation on completion.)

**Ceremony of Annihilation** (Doom Phase — spellbook): When performing a Ritual of Annihilation, may choose to pay nothing and instead EARN Power equal to the current ritual marker position, then advance the marker 1 step. No extra Doom or Elder Signs earned.

## IGOO Overlay Signature (for reference)

```scala
loyaltyCardIGOO(
  name: String,           // card name
  cost: String,           // awaken power cost as string
  combat: String,         // combat value as string
  hasSpellbook: Boolean,  // whether spellbook is active (passed as pattern var)
  obtainText: String,     // how to awaken
  ability: String,        // special ability name
  abilityPhase: String,   // when ability fires
  abilityText: String,    // ability description
  spellbookRequirement: String,  // SBR description
  spellbook: String,      // spellbook name
  spellbookPhase: String, // when spellbook fires
  spellbookText: String   // spellbook description
)
```

Case pattern: `case $("Tulzscha", spellbook : Boolean) =>`

The spellbook text is rendered faded when `hasSpellbook` is false (SBR not yet met), and normal when true.
