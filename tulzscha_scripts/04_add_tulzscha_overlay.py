#!/usr/bin/env python3
"""Script 4: Add Tulzscha info card to overlay.scala"""
import sys, os, re

sdir = os.environ.get("CWO_SCALA_DIR", "solo")
path = os.path.join(sdir, "overlay.scala")

with open(path, "r") as f:
    content = f.read()

tulzscha_case = '''        case $("Tulzscha") => igooCard(
            TulzschaCard.name,
            TulzschaCard.power,
            TulzschaCard.combat,
            "Pay 4 Power and place Tulzscha in an Area containing your controlled Gate and your Great Old One.",
            "Undying Flame",
            "Gather Power Phase",
            "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom than you; gain 1 Elder Sign if any Faction has more Elder Signs than you; gain 1 Power if any Faction has more Power than you.",
            "Spellbook Requirement",
            "As an Action, each enemy Faction gains 2 Power.",
            CeremonyOfAnnihilation.name,
            "Doom Phase",
            "When you perform a Ritual of Annihilation, you may pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs."
        )
'''

inserted = False
for marker in ['"Nyogtha"', '"Daoloth"', '"Abhoth"', '"Byatis"']:
    if marker not in content:
        continue
    # Find the case line containing this marker in the info function
    pattern = re.compile(r'case \$\(' + re.escape(marker) + r'\)[^\n]*\n')
    m = pattern.search(content)
    if not m:
        continue
    # Walk forward to end of this case block
    pos = m.end()
    lines = content[pos:pos+600].split('\n')
    block_len = 0
    for line in lines:
        block_len += len(line) + 1
        stripped = line.strip()
        if stripped.startswith('case ') or stripped == '}':
            break
    insert_pos = pos + block_len - (len(lines[-1]) + 1)  # before the next case
    content = content[:insert_pos] + tulzscha_case + content[insert_pos:]
    print(f"OK: Added Tulzscha igooCard case after {marker}")
    inserted = True
    break

if not inserted:
    print("ERROR: Could not find any IGOO info case in overlay.scala")
    print("Please manually add the following case to the info pattern match:")
    print()
    print(tulzscha_case)
    sys.exit(1)

with open(path, "w") as f:
    f.write(content)
print(f"\nDone: {path} updated.")
