#!/usr/bin/env python3
"""
Fix CthulhuWarsSolo.scala:
1. Add Tulzscha DrawRect after the Nyogtha DrawRect line
2. Add Tulzscha setup display line after the UseNyogtha display line
3. Fix the Tulzscha GC Deep DrawItem cases (they reference Nyogtha instead of Tulzscha)
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
path = os.path.join(sdir, "CthulhuWarsSolo.scala")

with open(path) as f:
    content = f.read()

changes = 0

# ── 1. DrawRect ───────────────────────────────────────────────────────────────
# Exact line from diagnostic:
# 'case Nyogtha       => DrawRect("n-nyogtha", |(tint), x - 40, y - 69, 81, 80)'
# Find it precisely and insert Tulzscha after it

nyogtha_drawrect = re.compile(r'([ \t]+case Nyogtha[ \t]+=> DrawRect\("n-nyogtha"[^\n]+)\n')
m = nyogtha_drawrect.search(content)
if m:
    indent = re.match(r'^([ \t]*)', m.group(1)).group(1)
    if 'case Tulzscha' not in content[m.start():m.start()+200]:
        new_line = indent + 'case Tulzscha         => DrawRect("n-tulzscha", |(tint), x - 35, y - 75, 70, 85)\n'
        content = content[:m.end()] + new_line + content[m.end():]
        print("OK: Added Tulzscha DrawRect")
        changes += 1
    else:
        print("OK: Tulzscha DrawRect already present")
else:
    print("WARNING: Could not find Nyogtha DrawRect line — add manually:")
    print('    case Tulzscha         => DrawRect("n-tulzscha", |(tint), x - 35, y - 75, 70, 85)')

# ── 2. Setup display line ─────────────────────────────────────────────────────
# Exact line from diagnostic:
# '.$("Variants" -> ("Use " + NyogthaCard.short + " (" + setup.get(UseNyogtha).?("yes").|("no").hl + ")")) ++'
display_pattern = re.compile(
    r'([ \t]*\.\$\("Variants" -> \("Use " \+ NyogthaCard\.short \+ " \(" \+ setup\.get\(UseNyogtha\)[^\n]+\)\)\) \+\+)\n'
)
matches = list(display_pattern.finditer(content))
if matches:
    # Insert after each occurrence (there are 2 — online and solo setup blocks)
    offset = 0
    inserted = 0
    for m in matches:
        indent = re.match(r'^([ \t]*)', m.group(1)).group(1)
        new_line = indent + '.$("Variants" -> ("Use " + TulzschaCard.short + " (" + setup.get(UseTulzscha).?("yes").|("no").hl + ")")) ++\n'
        pos = m.end() + offset
        # Check not already there
        next_100 = content[pos:pos+100]
        if 'UseTulzscha' not in next_100:
            content = content[:pos] + new_line + content[pos:]
            offset += len(new_line)
            inserted += 1
    print(f"OK: Added Tulzscha setup display line ({inserted} occurrence(s))")
    if inserted > 0:
        changes += 1
else:
    print("WARNING: Could not find UseNyogtha setup display line")

# ── 3. Fix Tulzscha GC Deep DrawItem cases (they say Nyogtha instead of Tulzscha) ──
# From diagnostic output these wrong lines exist:
# 'case (Cthulhu, Tulzscha) => DrawItem(null, f, Nyogtha, Alive, $, 86 + last.x, 6 + last.y)'
# 'case (Abhoth, Tulzscha) => DrawItem(null, f, Nyogtha, Alive, $, 81 + last.x, last.y)'
# 'case (Daoloth, Tulzscha) => DrawItem(null, f, Nyogtha, Alive, $, 86 + last.x, last.y)'
# 'case (Nyogtha, Tulzscha) => DrawItem(null, f, Nyogtha, Alive, $, 80 + last.x, last.y)'
# They should reference Tulzscha not Nyogtha as the drawn unit

wrong_pattern = re.compile(r'(case \([^,\n]+,\s*Tulzscha\) => DrawItem\(null, f,\s*)Nyogtha(\s*,\s*Alive)')
fixed = wrong_pattern.sub(r'\1Tulzscha\2', content)
if fixed != content:
    num_fixed = len(wrong_pattern.findall(content))
    content = fixed
    print(f"OK: Fixed {num_fixed} Tulzscha DrawItem cases (were pointing to Nyogtha)")
    changes += 1
else:
    print("OK: Tulzscha DrawItem cases already correct (or not present)")

# ── 4. Also need (Tulzscha, X) DrawItem cases for units that follow Tulzscha ──
# From the Nyogtha cases we can see (Nyogtha, Starspawn), (Nyogtha, Shoggoth) etc.
# We need equivalent (Tulzscha, X) cases. Check if they exist already.
if '(Tulzscha, Starspawn)' not in content:
    # Find the (Nyogtha, Starspawn) block and add Tulzscha equivalents
    # Pattern: a block of (Nyogtha, X) => DrawItem lines
    nyogtha_followers = re.compile(
        r'((?:[ \t]+case \(Nyogtha,\s*\w+\)[^\n]+\n)+)'
    )
    m = nyogtha_followers.search(content)
    if m:
        block = m.group(1)
        tulzscha_block = block.replace('(Nyogtha,', '(Tulzscha,')
        insert_pos = m.end()
        content = content[:insert_pos] + tulzscha_block + content[insert_pos:]
        num_lines = block.count('\n')
        print(f"OK: Added {num_lines} (Tulzscha, X) DrawItem follower cases")
        changes += 1
    else:
        print("INFO: Could not find (Nyogtha, X) follower block — may not be needed")

with open(path, "w") as f:
    f.write(content)
print(f"\nDone: {path} updated ({changes} change group(s)).")
