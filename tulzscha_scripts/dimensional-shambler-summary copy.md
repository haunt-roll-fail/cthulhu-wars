# Dimensional Shambler - Implementation Summary

## Monster Profile

```
┌─────────────────────────────────────────────────┐
│          DIMENSIONAL SHAMBLER                   │
├─────────────────────────────────────────────────┤
│ Loyalty Card Cost:  2 Doom                      │
│ Summon Cost:        2 Power                     │
│ Combat Value:       2                           │
│ Quantity:           3 per deck                  │
├─────────────────────────────────────────────────┤
│ Special Ability:    Walk Between Worlds         │
│ Type:               Ongoing                     │
│                                                 │
│ Placement: Faction Card → Board via Action     │
│ Duration:  Remain on Map (until Killed)        │
└─────────────────────────────────────────────────┘
```

---

## Implementation Overview

### Files Modified: 7

| File | Changes | Complexity |
|------|---------|-----------|
| `NeutralMonsters.scala` | 4 new objects | Low |
| `Game.scala` | 3 changes (combat, option, list) | Low |
| `Battle.scala` | None | — |
| `overlay.scala` | 1 info case | Low |
| `CthulhuWarsSolo.scala` | 6 changes in 2 blocks | Medium |
| `index.html` | 3 image tags | Low |
| `webp/` directory | 2 image files | N/A |

---

## Change Summary by File

### 1. NeutralMonsters.scala
```
Status: [████████] Complete
Lines:  4 new objects
Type:   Object Declarations
```

Add Scala case objects for:
- `DimensionalShamblerCard` — Loyalty card definition
- `DimensionalShamblerIcon` — Token icon representation
- `DimensionalShamblerUnit` — Board unit (cost: 2 power)
- `DimensionalShamblerHold` — Faction card holding token

---

### 2. Game.scala
```
Status: [████████] Complete
Lines:  3 changes across ~30 lines
Type:   Logic & Registration
```

Changes:
- **Line ~270**: Add combat contribution in `neutralStrength()` → `units(DimensionalShamblerUnit).num * 2`
- **Line ~770**: Define `UseDimensionalShamblers` case object
- **Line ~800**: Register in `GameOptions.all` list

---

### 3. Battle.scala
```
Status: [██████████] Complete (No Changes)
Lines:  0 changes
Type:   N/A
```

Dimensional Shambler has no battle-phase special ability. No modifications needed.

---

### 4. overlay.scala
```
Status: [████████] Complete
Lines:  1 case in pattern match
Type:   UI Definition
```

Add info card case with:
- Card title: "Dimensional Shambler"
- Quantity: 3
- Cost: 2
- Combat: 2
- Ability: "Walk Between Worlds" (Ongoing phase)
- Detailed ability text with mechanics

---

### 5. CthulhuWarsSolo.scala
```
Status: [████████████████] Medium Complexity
Lines:  6 changes in 2 mirrored blocks
Type:   UI Setup & Rendering
```

**Block 1 (Online Setup UI)**:
- A. Add display line (after StarVampire)
- B. Add to enable-all list (in setup.options ++= line)
- C. Add toggle handler (separate block for n -= 1)

**Block 2 (Solo Setup UI)**:
- A. Add display line (duplicate)
- B. Add to enable-all list (duplicate)
- C. Add toggle handler (duplicate)

**Shared Locations**:
- D. DrawRect at ~line 799
- E. GC sort order at ~line 1180
- F. GC DrawItem cases at ~line 1300 (1 full block + 1 Filth case)

---

### 6. index.html
```
Status: [████████] Complete
Lines:  3 image tags
Type:   Asset Registration
```

Add image assets:
- Board token: `n-dimensional-shambler` (webp)
- Icon: `dimensional-shambler-icon` (svg)
- Info card: `info:n-dimensional-shambler` (svg)

---

### 7. Image Files
```
Status: [████████] Pending (Placeholder)
Files:  2 required
Type:   Asset Files
```

Required files:
- `solo/webp/images/n-dimensional-shambler.webp` (70×85px, board token)
- `solo/webp/info/n-dimensional-shambler.svg` (info card icon)

**Download placeholders**:
```bash
curl -o solo/webp/images/n-dimensional-shambler.webp \
  https://cwo.im/hrf/webp/images/n-star-vampire.webp

curl -o solo/webp/info/n-dimensional-shambler.svg \
  https://cwo.im/hrf/webp/info/n-star-vampire.svg
```

---

## Game Flow Integration

### Acquisition Phase
```
Player selects "Use Dimensional Shamblers" in setup menu
        ↓
Loyalty card obtained (2 Doom cost)
        ↓
1 Dimensional Shambler placed on Faction Card
```

### Ongoing Gameplay
```
During Action Phase (any player's action)
        ↓
Player may place Shambler(s) from Faction Card to Board
        ↓
Placed Shambler(s) remain on map (normal unit until Killed)
        ↓
Unit can be moved, attacked, or submerged like any other unit
```

### Battle Resolution
```
Dimensional Shambler contributes 2 combat value to battles
        ↓
No special ability modifiers applied
        ↓
Standard casualty allocation after battle
```

### Great Cthulhu Submerge
```
GC submerges with Dimensional Shambler (and others)
        ↓
GC Deep rendering shows Shambler at sorted position (13)
        ↓
Shambler position relative to other units defined in DrawItem cases
```

---

## Code Patterns Used

### Pattern 1: Loyalty Card + Icon + Unit + Hold
```scala
// Card (obtainability)
case object DimensionalShamblerCard extends NeutralMonsterLoyaltyCard(...)

// Icon (UI representation)
case object DimensionalShamblerIcon extends UnitClass(... Token ...)

// Unit (board presence)
case object DimensionalShamblerUnit extends UnitClass(... Monster ...) with NeutralMonster

// Hold token (Faction Card storage)
case object DimensionalShamblerHold extends UnitClass(... Token ...)
```

### Pattern 2: Game Option Registration
```scala
// Define option
case object UseDimensionalShamblers extends LoyaltyCardGameOption(...) with NeutralMonsterOption

// Register globally
GameOptions.all += UseDimensionalShamblers
```

### Pattern 3: Combat Strength Calculation
```scala
units(DimensionalShamblerUnit).num * 2 +   // Multiply unit count by combat value
```

### Pattern 4: UI Setup Toggle
```scala
n -= 1
if (n == 0) {
    setup.toggle(UseDimensionalShamblers)
    setupQuestions()
}
```

### Pattern 5: Board Rendering
```scala
case DimensionalShamblerUnit => DrawRect("n-dimensional-shambler", |(tint), x - 35, y - 75, 70, 85)
```

### Pattern 6: GC Deep Rendering
```scala
case (Cthulhu, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 79 + last.x, 6 + last.y)
case (Abhoth, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 63 + last.x, last.y)
// ... (one case per GC variant + self-with-self case)
```

---

## Critical Implementation Points

### ✓ Four Objects Required
Must create all four case objects in NeutralMonsters.scala:
1. Card (loyalty card)
2. Icon (token display)
3. Unit (board presence)
4. Hold (Faction card storage)

Forgetting any causes undefined reference errors.

### ✓ Combat Value Placement
**Correct**: Combat value in `NeutralMonsterLoyaltyCard` constructor
**Wrong**: Combat value in `UnitClass` constructor

The third parameter of `UnitClass` is the **summon cost** (2 power), not combat (2).

### ✓ Separate Toggle Handlers
Each setup menu option needs its own complete `n -= 1 / if (n == 0)` block.
**DO NOT merge multiple options into one block.**

This causes the game to hang if a handler is skipped.

### ✓ Two Mirrored Blocks
Changes A, B, C must be applied **twice** in CthulhuWarsSolo.scala:
- Once in the online setup UI block
- Once in the solo setup UI block

Changes D, E, F are shared (only once).

### ✓ GC Deep Completeness
When adding a new unit to GC Deep rendering:
1. Add to sort order (defines position in stack)
2. Add one case per GC variant (Cthulhu, Abhoth, etc.)
3. Add self-with-self case
4. Add to Filth cases (for when Filth appears after)

Missing any causes a **crash when GC submerges**.

### ✓ Image Asset Paths
Must match exactly between:
- `CthulhuWarsSolo.scala` DrawRect id: `"n-dimensional-shambler"`
- `index.html` img id: `"n-dimensional-shambler"`

Mismatches result in missing visuals on board.

---

## Testing Checklist

- [ ] **Compilation**: `sbt fastOptJS` completes without errors
- [ ] **Setup Menu**: "Use Dimensional Shambler" option appears in Variants
- [ ] **Toggle**: Can select/deselect the option
- [ ] **Info Card**: Popup shows correct stats (2/2, 3 quantity, combat 2)
- [ ] **Ability Text**: "Walk Between Worlds" description displays correctly
- [ ] **Board Display**: Units render with correct image and position
- [ ] **Combat**: Units contribute 2 to faction battle strength
- [ ] **GC Submerge**: No crash when GC submerges with Shamblers
- [ ] **GC Deep Render**: Shamblers appear in correct positions relative to GC unit
- [ ] **Game State**: Units can be moved, attacked, killed normally
- [ ] **No Hangs**: Setup menu doesn't freeze or hang

---

## Reference Comparison

### vs. Voonith (Most Similar)
| Feature | Voonith | Shambler |
|---------|---------|----------|
| Combat | 1 | 2 |
| Quantity | 2 | 3 |
| Summon Cost | 3 power | 2 power |
| Battle Ability | Yes | No |
| Placement | Direct to board | Via Faction Card |

### vs. Star Vampire (Most Complex)
| Feature | Star Vampire | Shambler |
|---------|------------|----------|
| Combat | 3 | 2 |
| Quantity | 1 | 3 |
| Type | Monster | Monster |
| Battle Ability | Yes | No |
| Setup Requirements | All steps | All steps |

---

## Estimated Implementation Time

| Task | Time | Notes |
|------|------|-------|
| Code changes | 30-45 min | Follow snippets reference exactly |
| Image setup | 5-10 min | Download placeholders from server |
| Compilation | 5-10 min | First compile may be slow |
| Testing | 10-15 min | Verify all 10 checklist items |
| Debugging (if needed) | 15-30 min | Most common: toggle handlers or GC cases |
| **Total** | **60-110 min** | |

---

## Notes for Developers

1. **Faction Card Mechanic**: The game currently does not programmatically track figures on Faction Cards. Placement is a manual player action with no server-side validation.

2. **No Battle Ability**: Unlike Voonith (Vicious) and Star Vampire, Dimensional Shambler has no special rule that triggers during Battle resolution.

3. **Scaling**: With 3 units per deck and a 2-power summon cost, Shamblers can appear in larger quantities than other neutral monsters (which typically have 1-2).

4. **GC Deep Rendering**: 15 total cases required (9 GC types + 1 self + 4 other Shamblers + 1 Filth after). This is normal for a multi-unit monster.

5. **Image Dimensions**: Board token DrawRect specifies 70×85 pixels. Download placeholder images may differ; tune offsets as needed for correct positioning.

---

## Quick Start Command

```bash
# From repository root
cd ~/cthulhu-wars/solo && sbt fastOptJS && \
cd ~/cthulhu-wars/online && \
lsof -ti :999 | xargs kill -9 2>/dev/null; \
rm -f cwo.lck && \
sbt "run drop-create-run cwo http://localhost:999/ 999"
```

Then: Open `http://localhost:999/` and press **Cmd+Shift+R**
