#!/usr/bin/env python3
"""
Fix C: Add Ceremony of Annihilation ritual hook to Game.scala.

From script 07 output we know:
  - RitualAction(f, cost, k) in Game.scala
  - The ritual perform case is at char 73591

Run from anywhere — auto-detects repo.
"""
import sys, os, re

def find_igoos_dir():
    for start in [os.path.dirname(os.path.abspath(__file__)),
                  os.path.expanduser("~/cthulhu-wars"), os.path.expanduser("~")]:
        for dirpath, dirnames, filenames in os.walk(start):
            if "IGOOs.scala" in filenames:
                return dirpath
            dirnames[:] = [d for d in dirnames if not d.startswith('.') and d not in ('node_modules','target')]
    return None

sdir = os.environ.get("CWO_SCALA_DIR") or find_igoos_dir()

# ── Step 1: Add CeremonyOfAnnihilationChoiceAction to IGOOs.scala ─────────────
igoos_path = os.path.join(sdir, "IGOOs.scala")
with open(igoos_path) as f:
    igoos = f.read()

action_class = '''
case class CeremonyOfAnnihilationChoiceAction(self : Faction) extends OptionFactionAction(
    "Use " + CeremonyOfAnnihilation.styled(self) + " (earn Power, no Doom/ES)"
) with Soft'''

if 'CeremonyOfAnnihilationChoiceAction' in igoos:
    print("OK: CeremonyOfAnnihilationChoiceAction already exists in IGOOs.scala")
else:
    # Add after TulzschaGivePowerAction
    marker = 'case class TulzschaGivePowerAction(self : Faction)'
    if marker in igoos:
        idx = igoos.index(marker)
        eol = igoos.index('\n', idx)
        igoos = igoos[:eol] + action_class + igoos[eol:]
        print("OK: Added CeremonyOfAnnihilationChoiceAction to IGOOs.scala")
    else:
        print("WARNING: Could not find TulzschaGivePowerAction in IGOOs.scala")
        print("Manually add this action class:")
        print(action_class)

with open(igoos_path, "w") as f:
    f.write(igoos)

# ── Step 2: Add the perform handler in IGOOs.scala ────────────────────────────
# Find the ritual marker field name in Game.scala
game_path = os.path.join(sdir, "Game.scala")
with open(game_path) as f:
    game = f.read()

# Show context around the RitualAction perform case
print()
print("=== Game.scala RitualAction context ===")
ritual_m = re.search(r'case RitualAction\(f,\s*cost,\s*k\)', game)
if ritual_m:
    s = ritual_m.start()
    print(repr(game[max(0,s-200):s+800]))
    print()

# Find the ritual cost/marker field — look near 'ritualCost' or 'roaMarker'
print("=== Searching for ritual marker field ===")
for kw in ['roaMarker', 'ritualMarker', 'annihilationStep', 'ritualStep',
           'ritualCost', 'currentCost', 'roaCost', 'markerPos', 'doomStep']:
    for m in re.finditer(re.escape(kw), game):
        s = m.start()
        line_s = game.rindex('\n', 0, s) + 1
        line_e = game.index('\n', s)
        print(f"  '{kw}' at {s}: {repr(game[line_s:line_e])}")

# Show the full RitualAction body to understand what fields are available
print()
print("=== Full RitualAction case body ===")
if ritual_m:
    s = ritual_m.start()
    # Find the end of this case (next top-level case or closing brace)
    body = game[s:s+1500]
    print(repr(body))

# ── Step 3: Add the intercept in IGOOs.scala perform() ────────────────────────
# We need to intercept BEFORE Game.scala handles it,
# but IGOOs.scala's perform() is called first via the expansion system.
# So we add a case in IGOOs perform() that matches RitualAction and offers the choice.

with open(igoos_path) as f:
    igoos = f.read()

if 'CeremonyOfAnnihilationChoiceAction' not in igoos:
    print("ERROR: Action class not found in IGOOs.scala — cannot continue")
    sys.exit(1)

# The intercept: add before "// TULZSCHA" section in perform()
intercept = '''        // TULZSCHA - Ceremony of Annihilation ritual intercept
        case RitualAction(self, cost, k) if self.has(Tulzscha) && self.upgrades.has(CeremonyOfAnnihilation) =>
            Ask(self)
                .option(CeremonyOfAnnihilationChoiceAction(self), "Ceremony of Annihilation (earn Power, skip Doom/ES)")
                .option(RitualAction(self, cost, k), "Perform ritual normally")
                .cancel

        case CeremonyOfAnnihilationChoiceAction(self) =>
            val markerPos = game.ritualCost
            self.power += markerPos
            self.log("used", CeremonyOfAnnihilation.styled(self), "and earned", markerPos.power)
            EndAction(self)

'''

marker = '        // TULZSCHA\n        case TulzschaGivePowerMainAction'
if 'CeremonyOfAnnihilationChoiceAction(self) =>' in igoos:
    print("OK: Ceremony of Annihilation intercept already in IGOOs.scala")
elif marker in igoos:
    igoos = igoos.replace(marker, intercept + marker, 1)
    print("OK: Added Ceremony of Annihilation intercept to IGOOs.scala")
    print()
    print("NOTE: The ritual marker field name used is 'game.ritualCost'.")
    print("Run the compile and check for errors. If it fails, look at what field")
    print("name the RitualAction body uses for the current marker step and update")
    print("that line in IGOOs.scala's CeremonyOfAnnihilationChoiceAction handler.")
else:
    print("WARNING: Could not find TULZSCHA section in IGOOs.scala perform()")
    print("Add this block manually before the TulzschaGivePowerMainAction case:")
    print()
    print(intercept)

with open(igoos_path, "w") as f:
    f.write(igoos)

print(f"\nDone. Compile with: cd {os.path.dirname(sdir)}/{os.path.basename(sdir)} && sbt fastOptJS")
