#!/usr/bin/env python3
"""Script 5: Add Tulzscha to CthulhuWarsSolo.scala"""
import sys, os, re

sdir = os.environ.get("CWO_SCALA_DIR", "solo")
path = os.path.join(sdir, "CthulhuWarsSolo.scala")

with open(path, "r") as f:
    content = f.read()

changes = 0

# ── 1. DrawRect ───────────────────────────────────────────────────────────────
drawrect_inserted = False
for t in ['case Nyogtha =>', 'case NyogthaIcon =>']:
    pattern = re.compile(r'([ \t]*)(' + re.escape(t) + r'[^\n]+DrawRect[^\n]+)\n')
    m = pattern.search(content)
    if m:
        indent = m.group(1)
        eol = m.end()
        insertion = indent + 'case Tulzscha => DrawRect("n-tulzscha", |(tint), x - 35, y - 75, 70, 85)\n'
        content = content[:eol] + insertion + content[eol:]
        print(f"OK: Added Tulzscha DrawRect after {t}")
        drawrect_inserted = True
        changes += 1
        break
if not drawrect_inserted:
    print("WARNING: Could not find Nyogtha DrawRect — add manually:")
    print('    case Tulzscha => DrawRect("n-tulzscha", |(tint), x - 35, y - 75, 70, 85)')

# ── 2. GC Deep sort order ─────────────────────────────────────────────────────
sort_match = re.search(r'([ \t]+case Nyogtha\s*=>[ \t]*)(\d+)', content)
if sort_match:
    nyogtha_num = int(sort_match.group(2))
    tulzscha_num = nyogtha_num + 1
    filth_num   = tulzscha_num + 1
    eol = content.index('\n', sort_match.end())
    indent = re.match(r'^([ \t]*)', sort_match.group(1)).group(1)
    content = content[:eol] + f'\n{indent}case Tulzscha =>     {tulzscha_num}' + content[eol:]
    print(f"OK: Added Tulzscha sort order ({tulzscha_num})")
    # Update Filth
    filth_m = re.search(r'([ \t]+case Filth\s*=>[ \t]*)(\d+)', content)
    if filth_m:
        content = content[:filth_m.start(2)] + str(filth_num) + content[filth_m.end(2):]
        print(f"OK: Updated Filth sort order to {filth_num}")
    changes += 1
else:
    print("WARNING: Could not find Nyogtha sort order — add manually")

# ── 3. GC Deep DrawItem cases ─────────────────────────────────────────────────
# Find block of (X, Nyogtha) DrawItem cases and duplicate for Tulzscha
nyogtha_di = re.search(r'((?:[ \t]+case \([^,\n]+,\s*Nyogtha\)[^\n]+\n)+)', content)
if nyogtha_di:
    block = nyogtha_di.group(1)
    tulzscha_block = block.replace(', Nyogtha)', ', Tulzscha)')
    # Add (Tulzscha, Nyogtha) and (Tulzscha, Tulzscha)
    ind = re.match(r'^([ \t]*)', block).group(1)
    extra = (ind + 'case (Tulzscha, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 65 + last.x, last.y)\n' +
             ind + 'case (Tulzscha, Tulzscha) => DrawItem(null, f, Tulzscha, Alive, $, 65 + last.x, last.y)\n')
    insert_pos = nyogtha_di.end()
    content = content[:insert_pos] + tulzscha_block + extra + content[insert_pos:]
    print("OK: Added Tulzscha GC Deep DrawItem cases")
    changes += 1
else:
    print("WARNING: Could not find Nyogtha GC Deep DrawItem block — add manually")

# ── 4. Setup display lines ─────────────────────────────────────────────────────
display_pattern = re.compile(r'([^\n]*UseNyogtha[^\n]*NyogthaCard[^\n]*\n)')
matches = list(display_pattern.finditer(content))
if matches:
    offset = 0
    for m in matches:
        line = m.group(1)
        new_line = line.replace('UseNyogtha', 'UseTulzscha').replace('NyogthaCard', 'TulzschaCard')
        pos = m.end() + offset
        content = content[:pos] + new_line + content[pos:]
        offset += len(new_line)
    print(f"OK: Added Tulzscha setup display line ({len(matches)}x)")
    changes += 1
else:
    print("WARNING: Could not find UseNyogtha setup display line — add manually")

# ── 5. Enable-all list ────────────────────────────────────────────────────────
enable_all = re.compile(r'(setup\.options \+\+= \$\([^)]*)(UseNyogtha)(\))')
count = 0
for m in enable_all.finditer(content):
    old = m.group(0)
    new = m.group(1) + m.group(2) + ', UseTulzscha' + m.group(3)
    content = content.replace(old, new, 1)
    count += 1
if count:
    print(f"OK: Added UseTulzscha to enable-all list ({count}x)")
    changes += 1
else:
    print("WARNING: Could not find enable-all list — add UseTulzscha manually")

# ── 6. Toggle handlers ────────────────────────────────────────────────────────
toggle_pattern = re.compile(
    r'([ \t]*n -= 1\n[ \t]*if \(n == 0\) \{\n[ \t]*setup\.toggle\(UseNyogtha\)\n[ \t]*setupQuestions\(\)\n[ \t]*\})'
)
matches = list(toggle_pattern.finditer(content))
if matches:
    offset = 0
    for m in matches:
        block = m.group(1)
        tulzscha_block = '\n' + block.replace('UseNyogtha', 'UseTulzscha')
        pos = m.end() + offset
        content = content[:pos] + tulzscha_block + content[pos:]
        offset += len(tulzscha_block)
    print(f"OK: Added Tulzscha toggle handler ({len(matches)}x)")
    changes += 1
else:
    print("WARNING: Could not find UseNyogtha toggle — add manually after each UseNyogtha block:")
    print("""                n -= 1
                if (n == 0) {
                    setup.toggle(UseTulzscha)
                    setupQuestions()
                }""")

with open(path, "w") as f:
    f.write(content)
print(f"\nDone: {path} updated ({changes} change groups).")
