#!/usr/bin/env python3
"""
Script 7: Diagnostic — find the Ritual of Annihilation perform logic and
print exactly what to add for Ceremony of Annihilation.
"""
import os, re

sdir = os.environ.get("CWO_SCALA_DIR", "solo")
repo = os.environ.get("CWO_REPO_ROOT", ".")

ritual_keywords = [
    "RitualOfAnnihilation", "ritualOfAnnihilation",
    "Ritual of Annihilation", "ritualCost", "performRitual",
    "DoRitual", "RitualAction", "RoAMarker", "roaMarker",
    "annihilation", "Annihilation",
]

print("Searching for Ritual of Annihilation logic...\n")
found = {}
for fname in os.listdir(sdir):
    if not fname.endswith(".scala"):
        continue
    fpath = os.path.join(sdir, fname)
    with open(fpath) as f:
        text = f.read()
    hits = [(kw, text.count(kw)) for kw in ritual_keywords if kw in text]
    if hits:
        found[fpath] = (text, hits)

if not found:
    print("ERROR: No Scala file in", sdir, "mentions any ritual keyword.")
    print("You will need to manually locate the Ritual of Annihilation perform() case.")
else:
    for fpath, (text, hits) in sorted(found.items(), key=lambda x: -sum(v for _,v in x[1][1])):
        print(f"FILE: {fpath}")
        for kw, count in hits:
            print(f"  '{kw}': {count} occurrences")
        # Show context around perform/case for ritual
        for m in re.finditer(r'case\s+\w*[Rr]itual\w*\s*\(', text):
            s = m.start()
            snippet = text[max(0,s-20):s+300]
            print(f"\n  Ritual action case at char {s}:")
            print("  " + repr(snippet))
        print()

print("=" * 70)
print("CEREMONY OF ANNIHILATION — what to add")
print("=" * 70)
print("""
Add this action class near the other Tulzscha action classes in IGOOs.scala:

    case class CeremonyOfAnnihilationChoiceAction(self : Faction, normalCost : Int) extends OptionFactionAction(
        "Use " + CeremonyOfAnnihilation.styled(self) + " (earn Power instead of paying)"
    ) with Soft

Find the perform() case that handles the Ritual of Annihilation payment
(probably something like 'case RitualAction(self, cost)' or similar).

BEFORE that case, add a guard:

    case RitualAction(self, cost, ...)                        // <-- match the exact case class
        if self.has(Tulzscha) && self.upgrades.has(CeremonyOfAnnihilation) =>
            Ask(self)
                .option(CeremonyOfAnnihilationChoiceAction(self, cost))
                .option(...)    // the normal ritual option
                .cancel

Then add a handler for the Ceremony choice:

    case CeremonyOfAnnihilationChoiceAction(self, _) =>
        // Earn power = current ritual marker position; advance marker 1 step; no Doom/ES bonus
        val markerPos = game.roaMarker   // <-- use the actual field name from your Game.scala
        self.power += markerPos
        game.roaMarker += 1              // <-- use the actual field name
        self.log("used", CeremonyOfAnnihilation.styled(self), "and earned", markerPos.power)
        // Do NOT advance doom or award elder signs
        EndAction(self)

KEY: check Game.scala for the field that tracks the Ritual of Annihilation
marker position (search for 'roaMarker', 'ritualMarker', 'annihilationStep',
'doomTrack', or similar). Use that exact field name above.
""")
