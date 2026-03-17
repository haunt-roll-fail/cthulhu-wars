#!/usr/bin/env python3
"""
Fix Ceremony of Annihilation in IGOOs.scala.

From Game.scala output we know:
- RitualAction(f, cost, k) where k is doom-per-gate multiplier
- game.ritualCost = min(10, ritualTrack(ritualMarker))  — this is the COST, not what we earn
- The faction earns 'doom = valid.num * k' doom normally
- For Ceremony, they earn Power = ritualCost (current marker step cost), pay nothing, advance marker

The intercept must:
1. Fire BEFORE the standard RitualAction handler in Game.scala
2. Only offer the choice if CeremonyOfAnnihilation is active
3. On choice: earn power = ritualCost, advance ritualMarker, NO doom/ES bonus

Since IGOOs.scala perform() runs via expansion dispatch BEFORE Game.scala,
the RitualAction guard in IGOOs perform() will intercept correctly.

Also fix: the existing intercept used 'game.ritualCost' but in Game.scala
ritualCost is a def on the Game class itself (not on 'game'), so in IGOOs.scala
it needs to be called via the implicit game parameter.
"""
import sys, os, re

def find_sdir():
    for start in [os.path.dirname(os.path.abspath(__file__)),
                  os.path.expanduser("~/cthulhu-wars"), os.path.expanduser("~")]:
        for dp, dns, fns in os.walk(start):
            if "IGOOs.scala" in fns: return dp
            dns[:] = [d for d in dns if not d.startswith('.') and d not in ('node_modules','target')]
    return None

sdir = os.environ.get("CWO_SCALA_DIR") or find_sdir()
path = os.path.join(sdir, "IGOOs.scala")

with open(path) as f:
    content = f.read()

# ── Remove any previously inserted intercept (may have wrong field names) ────
old_intercept = re.compile(
    r'[ \t]*// TULZSCHA - Ceremony of Annihilation ritual intercept\n'
    r'[ \t]*case RitualAction\(self, cost, k\)[^\n]*\n'
    r'(?:[ \t]*[^\n]*\n)*?'
    r'[ \t]*EndAction\(self\)\n\n',
    re.DOTALL
)
cleaned = old_intercept.sub('', content)
if cleaned != content:
    content = cleaned
    print("OK: Removed old intercept block")

# ── Remove old CeremonyOfAnnihilationChoiceAction perform handler if present ─
old_choice_handler = re.compile(
    r'[ \t]*case CeremonyOfAnnihilationChoiceAction\(self\)[^\n]*\n'
    r'(?:[ \t]*[^\n]*\n)*?'
    r'[ \t]*EndAction\(self\)\n',
    re.DOTALL
)
cleaned = old_choice_handler.sub('', content)
if cleaned != content:
    content = cleaned
    print("OK: Removed old choice handler")

# ── Build correct intercept ───────────────────────────────────────────────────
# ritualCost is a def on Game (implicit game), accessible as game.ritualCost
# ritualMarker is a var on Game, accessible as game.ritualMarker
correct_intercept = '''        // TULZSCHA - Ceremony of Annihilation ritual intercept
        case RitualAction(self, cost, k) if self.has(Tulzscha) && self.upgrades.has(CeremonyOfAnnihilation) =>
            Ask(self)
                .option(CeremonyOfAnnihilationChoiceAction(self), "Ceremony of Annihilation (earn " + game.ritualCost + " Power, skip Doom/ES)")
                .option(RitualAction(self, cost, k), "Perform ritual normally (" + cost + " Power)")
                .cancel

        case CeremonyOfAnnihilationChoiceAction(self) =>
            val earned = game.ritualCost
            self.power += earned
            if (game.ritualTrack(game.ritualMarker) != 999)
                game.ritualMarker += 1
            game.showROAT()
            self.acted = true
            self.satisfy(PerformRitual, "Perform Ritual of Annihilation")
            self.log("used", CeremonyOfAnnihilation.styled(self), "and earned", earned.power, "(no Doom or Elder Signs)")
            CheckSpellbooksAction(DoomAction(self))

'''

# Insert before the "// TULZSCHA" section for give-power action
marker = '        // TULZSCHA\n        case TulzschaGivePowerMainAction'
if marker not in content:
    # Fallback: insert before the final "// ..." catch-all
    marker = '        // ...\n        case _ => UnknownContinue'

if marker in content:
    content = content.replace(marker, correct_intercept + marker, 1)
    print("OK: Inserted correct Ceremony of Annihilation intercept")
else:
    print("ERROR: Could not find insertion point in IGOOs.scala perform()")
    print("Manually add this before TulzschaGivePowerMainAction:")
    print(correct_intercept)
    sys.exit(1)

with open(path, "w") as f:
    f.write(content)
print(f"Done: {path} updated.")
