#!/usr/bin/env python3
"""Script 1: Add Tulzscha objects to IGOOs.scala"""
import sys, os

sdir = os.environ.get("CWO_SCALA_DIR", "solo")
path = os.path.join(sdir, "IGOOs.scala")

with open(path, "r") as f:
    content = f.read()

# 1. Add loyalty card + icon after NyogthaIcon
old = 'case object NyogthaIcon extends UnitClass(Nyogtha.name + " Icon", Token, 0)'
new = old + '\n\ncase object TulzschaCard extends IGOOLoyaltyCard(TulzschaIcon, Tulzscha, power = 4, combat = 1)\ncase object TulzschaIcon extends UnitClass(Tulzscha.name + " Icon", Token, 0)'
if old not in content:
    print("ERROR: Could not find NyogthaIcon. Check repr:"); print(repr(content[content.find("NyogthaIcon")-10:content.find("NyogthaIcon")+80])); sys.exit(1)
content = content.replace(old, new, 1)
print("OK: Added TulzschaCard + TulzschaIcon")

# 2. Add Tulzscha UnitClass after Nyogtha
old = 'case object Nyogtha extends UnitClass("Nyogtha", GOO, 6) with IGOO'
new = old + '\ncase object Tulzscha extends UnitClass("Tulzscha", GOO, 4) with IGOO'
if old not in content:
    print("ERROR: Could not find Nyogtha UnitClass"); sys.exit(1)
content = content.replace(old, new, 1)
print("OK: Added Tulzscha UnitClass")

# 3. Add spellbook after NightmareWeb
old = 'case object NightmareWeb extends NeutralSpellbook("Nightmare Web")'
new = old + '\n\n// Tulzscha\ncase object CeremonyOfAnnihilation extends NeutralSpellbook("Ceremony of Annihilation")'
if old not in content:
    print("ERROR: Could not find NightmareWeb"); sys.exit(1)
content = content.replace(old, new, 1)
print("OK: Added CeremonyOfAnnihilation spellbook")

with open(path, "w") as f:
    f.write(content)
print(f"\nDone: {path} updated.")
