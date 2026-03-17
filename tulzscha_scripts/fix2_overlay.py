#!/usr/bin/env python3
"""
Fix overlay.scala — replace the wrongly-inserted Tulzscha case with the correct one.
The real signature is:
  loyaltyCardIGOO(name, cost, combat, hasSpellbook, obtainText,
                  ability, abilityPhase, abilityText,
                  spellbookRequirement,
                  spellbook, spellbookPhase, spellbookText)
And the case pattern is:  case $("Tulzscha", spellbook : Boolean) =>
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
path = os.path.join(sdir, "overlay.scala")

with open(path) as f:
    content = f.read()

# Remove any previously inserted Tulzscha case (bad ones from earlier runs)
bad_patterns = [
    # The igooCard(...) style inserted by fix_overlay.py
    re.compile(r'[ \t]*case \$\("Tulzscha"\) => igooCard\([^)]*\)\n'),
    # Multi-line igooCard
    re.compile(r'[ \t]*case \$\("Tulzscha"\) => igooCard\(.*?\)\n', re.DOTALL),
]
for pat in bad_patterns:
    new_content = pat.sub('', content)
    if new_content != content:
        content = new_content
        print("OK: Removed previously inserted bad Tulzscha case")
        break

# Verify Tulzscha not already correctly present
if 'case $("Tulzscha", spellbook : Boolean)' in content:
    print("OK: Correct Tulzscha case already present — nothing to do.")
    sys.exit(0)

# The correct Tulzscha case to insert
tulzscha_case = '        case $("Tulzscha", spellbook : Boolean) => loyaltyCardIGOO(TulzschaCard.name, "" + TulzschaCard.power, "" + TulzschaCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Tulzscha in the Area containing the Gate.", "Undying Flame", "Gather Power Phase", "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom than you; gain 1 Elder Sign if any Faction has more Elder Signs than you; gain 1 Power if any Faction has more Power than you.", "As an Action, each enemy Faction gains 2 Power.", "Ceremony of Annihilation", "Doom Phase", "When you perform a Ritual of Annihilation, you may choose to pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs.")\n'

# Find the last Nyogtha case line (the one that ends with ')') and insert after it
# The exact line (single line) ends with the closing paren
nyogtha_pattern = re.compile(r'(        case \$\("Nyogtha", spellbook : Boolean\) => loyaltyCardIGOO\([^\n]+\)\n)')
matches = list(nyogtha_pattern.finditer(content))
if not matches:
    print("ERROR: Could not find Nyogtha loyaltyCardIGOO case line")
    sys.exit(1)

# Use the LAST match (they're duplicated in multiple function contexts — insert after each)
# Actually insert after the last one only
m = matches[-1]
content = content[:m.end()] + tulzscha_case + content[m.end():]
print(f"OK: Inserted Tulzscha loyaltyCardIGOO case after last Nyogtha case")

with open(path, "w") as f:
    f.write(content)
print(f"Done: {path} updated.")
