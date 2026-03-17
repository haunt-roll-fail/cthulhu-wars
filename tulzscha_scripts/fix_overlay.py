#!/usr/bin/env python3
"""
Fix A: Add Tulzscha to overlay.scala by finding the actual IGOO pattern used.
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
if not sdir:
    print("ERROR: Cannot find scala source dir"); sys.exit(1)

path = os.path.join(sdir, "overlay.scala")
with open(path) as f:
    content = f.read()

# ── Diagnose: show every line mentioning Byatis/Abhoth/Daoloth/Nyogtha ────────
print("=== Searching overlay.scala for IGOO patterns ===")
for name in ["Byatis", "Abhoth", "Daoloth", "Nyogtha", "igooCard", "IGOO"]:
    for m in re.finditer(re.escape(name), content):
        s = m.start()
        # show the surrounding case line
        sol = content.rindex('\n', 0, s) + 1
        eol = content.index('\n', s)
        line = content[sol:eol]
        print(f"  '{name}' at {s}: {repr(line)}")

print()

# ── Find the actual insert point ──────────────────────────────────────────────
# Strategy 1: look for case containing Nyogtha (any form)
insert_pos = None
insert_label = None

# Try: case containing the string "Nyogtha" anywhere on the line, inside info/overlay function
for m in re.finditer(r'[^\n]*Nyogtha[^\n]*\n', content):
    line = m.group(0)
    # Skip lines that are just definitions (case object, etc)
    if 'case object' in line or 'class ' in line or '//' in line[:line.find('Nyogtha')]:
        continue
    # This is probably a case in the info match
    insert_pos = m.end()
    insert_label = f"after Nyogtha line: {repr(line.strip())}"
    break

# Strategy 2: find the last IGOO name mention before a closing brace
if insert_pos is None:
    for name in ["Daoloth", "Abhoth", "Byatis"]:
        for m in re.finditer(r'[^\n]*' + name + r'[^\n]*\n', content):
            line = m.group(0)
            if 'case object' in line or 'class ' in line:
                continue
            insert_pos = m.end()
            insert_label = f"after {name} line: {repr(line.strip())}"
            # don't break — keep the last one

if insert_pos is None:
    print("Could not find any insertion point.")
    print("Please paste the following manually into the info pattern match in overlay.scala:")
    print()
    print("""Paste after the Nyogtha/last IGOO case:

        case $("Tulzscha") => igooCard(TulzschaCard, self,
            "Pay 4 Power and place Tulzscha in an Area containing your controlled Gate and your Great Old One.",
            "Undying Flame", "Gather Power Phase",
            "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom; gain 1 Elder Sign if any Faction has more Elder Signs; gain 1 Power if any Faction has more Power.",
            "Spellbook Requirement",
            "As an Action, each enemy Faction gains 2 Power.",
            CeremonyOfAnnihilation, "Doom Phase",
            "When you perform a Ritual of Annihilation, you may pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs."
        )
""")
    sys.exit(1)

# ── Show a wider context so we can see the exact function signature used ──────
print(f"Best insert point: {insert_label}")
print()
print("Context (200 chars before insert point):")
print(repr(content[max(0, insert_pos-300):insert_pos+50]))
print()

# ── Now try to match the exact call style from surrounding context ────────────
# Look for igooCard calls near the insert point
nearby = content[max(0, insert_pos-2000):insert_pos+100]

# Find any igooCard( call and show its signature
igoo_call = re.search(r'igooCard\s*\([^)]{0,500}\)', nearby, re.DOTALL)
if igoo_call:
    print("Found igooCard call nearby:")
    print(repr(igoo_call.group(0)[:400]))
    print()

# Check what pattern the case line uses: case $("X"), case "X", case |(x), etc.
case_patterns = re.findall(r'(case\s+[^\n]{0,60}(?:Byatis|Abhoth|Daoloth|Nyogtha)[^\n]*)', content)
print("Case lines containing IGOO names:")
for p in case_patterns:
    print(f"  {repr(p)}")
print()

# ── Build the tulzscha case using the same style as what we found ─────────────
# We'll try multiple candidate formats and pick based on what's nearby

# Check which format is used
if '$("Nyogtha")' in nearby or '$("Abhoth")' in nearby:
    case_start = 'case $("Tulzscha")'
elif '"Nyogtha"' in nearby:
    case_start = 'case "Tulzscha"'
else:
    # Fall back to showing the raw context and asking user
    print("Cannot determine case format automatically.")
    print("Look at the case lines above and use the same format.")
    print("The body of the case should call igooCard(...) with the same arguments as other IGOOs.")
    sys.exit(1)

# Check igooCard signature — does it take (Card, faction, ...) or (name, power, combat, ...)?
if 'TulzschaCard' in nearby or 'NyogthaCard' in nearby or re.search(r'igooCard\s*\(\s*\w+Card', nearby):
    # card-object style
    tulzscha_case = f'''        {case_start} => igooCard(TulzschaCard, self,
            "Pay 4 Power and place Tulzscha in an Area containing your controlled Gate and your Great Old One.",
            "Undying Flame", "Gather Power Phase",
            "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom; gain 1 Elder Sign if any Faction has more Elder Signs; gain 1 Power if any Faction has more Power.",
            "Spellbook Requirement",
            "As an Action, each enemy Faction gains 2 Power.",
            CeremonyOfAnnihilation, "Doom Phase",
            "When you perform a Ritual of Annihilation, you may pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs."
        )
'''
else:
    # name/power/combat style (as in neutral monsters)
    tulzscha_case = f'''        {case_start} => igooCard(
            TulzschaCard.name, TulzschaCard.power, TulzschaCard.combat,
            "Pay 4 Power and place Tulzscha in an Area containing your controlled Gate and your Great Old One.",
            "Undying Flame", "Gather Power Phase",
            "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom; gain 1 Elder Sign if any Faction has more Elder Signs; gain 1 Power if any Faction has more Power.",
            "Spellbook Requirement",
            "As an Action, each enemy Faction gains 2 Power.",
            CeremonyOfAnnihilation.name, "Doom Phase",
            "When you perform a Ritual of Annihilation, you may pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs."
        )
'''

content = content[:insert_pos] + tulzscha_case + content[insert_pos:]
with open(path, "w") as f:
    f.write(content)
print(f"OK: Inserted Tulzscha case into overlay.scala")
print(f"Done: {path} updated.")
