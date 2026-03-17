#!/usr/bin/env python3
"""
Fix B: Add missing items to CthulhuWarsSolo.scala:
  1. Tulzscha DrawRect (board token rendering)
  2. Tulzscha setup display line

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
path = os.path.join(sdir, "CthulhuWarsSolo.scala")
with open(path) as f:
    content = f.read()

changes = 0

# ── 1. DrawRect ───────────────────────────────────────────────────────────────
print("=== Diagnosing DrawRect ===")

# Find all DrawRect lines to understand the pattern
drawrect_lines = [(m.start(), content[content.rindex('\n',0,m.start())+1:content.index('\n',m.start())])
                  for m in re.finditer(r'DrawRect\(', content)]
print(f"Found {len(drawrect_lines)} DrawRect lines")
for pos, line in drawrect_lines[:20]:
    print(f"  {repr(line.strip())}")

# Find the block that has GOO/monster DrawRect calls — look for case + DrawRect on same line
# or a match block with case objects
print()

# Search for any line with "Nyogtha" anywhere near DrawRect
for m in re.finditer(r'[^\n]*Nyogtha[^\n]*', content):
    line = m.group(0)
    print(f"Nyogtha line: {repr(line.strip())}")

print()

# Find all lines with "case Nyogtha" or similar
for m in re.finditer(r'[ \t]*case\s+Nyogtha\b[^\n]*', content):
    s = m.start()
    eol = content.index('\n', s)
    next200 = content[s:s+300]
    print(f"'case Nyogtha' at {s}: {repr(content[s:eol])}")
    print(f"  Next 200 chars: {repr(next200)}")
    print()

# ── Try to find the specific DrawRect match block by looking for GOO names ────
# The DrawRect for units is typically inside a function like drawUnit or similar
# Search for the function/match that handles rendering tokens
print("=== Looking for token render match block ===")
render_match = re.search(r'def\s+\w*[Dd]raw\w*[^\n]*\n.*?(?=def\s)', content, re.DOTALL)
# Instead look for multiple consecutive case + DrawRect lines
consec = re.search(r'((?:[ \t]+case \w+ =>[^\n]*DrawRect[^\n]*\n){2,})', content)
if consec:
    print("Found consecutive DrawRect case block:")
    print(repr(consec.group(0)[:600]))

    block = consec.group(0)
    # Check if Nyogtha or other GOOs are in here
    if 'Nyogtha' in block or 'Abhoth' in block or 'Daoloth' in block:
        # Insert Tulzscha after the last IGOO in this block
        last_igoo_in_block = None
        for name in ['Nyogtha', 'Daoloth', 'Abhoth', 'Byatis']:
            if name in block:
                last_igoo_in_block = name
                break
        if last_igoo_in_block:
            # Find that line in the full content
            pat = re.compile(r'([ \t]+case ' + last_igoo_in_block + r'[^\n]*DrawRect[^\n]*)\n')
            m2 = pat.search(content)
            if m2:
                indent = re.match(r'^([ \t]*)', m2.group(1)).group(1)
                insert_pos = m2.end()
                new_line = indent + 'case Tulzscha => DrawRect("n-tulzscha", |(tint), x - 35, y - 75, 70, 85)\n'
                content = content[:insert_pos] + new_line + content[insert_pos:]
                print(f"OK: Inserted Tulzscha DrawRect after {last_igoo_in_block}")
                changes += 1
    elif 'Filth' in block or 'StarVampire' in block or 'Voonith' in block:
        # It's the neutral monster block — insert Tulzscha before the first neutral
        first_line_end = content.index('\n', consec.start()) + 1
        indent = re.match(r'^([ \t]*)', block).group(1)
        new_line = indent + 'case Tulzscha => DrawRect("n-tulzscha", |(tint), x - 35, y - 75, 70, 85)\n'
        content = content[:consec.start()] + new_line + content[consec.start():]
        print("OK: Inserted Tulzscha DrawRect at start of neutral DrawRect block")
        changes += 1
else:
    print("Could not find consecutive DrawRect case block.")
    # Show all DrawRect lines for manual inspection
    print("Manual action needed — add this line near other IGOO/GOO DrawRect cases:")
    print('    case Tulzscha => DrawRect("n-tulzscha", |(tint), x - 35, y - 75, 70, 85)')

# ── 2. Setup display line ─────────────────────────────────────────────────────
print()
print("=== Diagnosing setup display line ===")

# Show all lines mentioning UseNyogtha
for m in re.finditer(r'[^\n]*UseNyogtha[^\n]*', content):
    print(f"UseNyogtha line: {repr(m.group(0).strip())}")

print()

# The setup display for IGOOs likely uses a different pattern.
# Show lines mentioning Nyogtha in setup context
for m in re.finditer(r'[^\n]*[Nn]yogtha[^\n]*', content):
    line = m.group(0)
    if 'setup' in line or 'option' in line.lower() or 'Variants' in line or 'Use ' in line:
        print(f"Setup-related Nyogtha line: {repr(line.strip())}")

# Try to find the setup options display block
# Look for the pattern used for other IGOOs in setup
for name in ['Nyogtha', 'Daoloth', 'Abhoth', 'Byatis']:
    pattern = re.compile(r'[^\n]*Use' + name + r'[^\n]*\n')
    for m in pattern.finditer(content):
        line = m.group(0)
        if 'toggle' in line or 'option' in line.lower() or 'setup' in line:
            continue  # skip toggle handlers
        # This is likely the display line
        print(f"Candidate display line for {name}: {repr(line.strip())}")
        new_line = line.replace('Use' + name, 'UseTulzscha').replace(name + 'Card', 'TulzschaCard').replace(
            '"' + name + '"', '"Tulzscha"').replace("'" + name + "'", "'Tulzscha'")
        insert_pos = m.end()
        content = content[:insert_pos] + new_line + content[insert_pos:]
        print(f"OK: Inserted Tulzscha setup display line after Use{name}")
        changes += 1
        break
    if changes >= 3:  # stop after finding display lines
        break

with open(path, "w") as f:
    f.write(content)
print(f"\nDone: {path} updated ({changes} change(s) applied).")
