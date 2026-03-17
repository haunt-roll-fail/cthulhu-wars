#!/usr/bin/env python3
"""Fix the missing Tulzscha setup display line in CthulhuWarsSolo.scala"""
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

if 'UseTulzscha).?("yes")' in content:
    print("OK: Tulzscha setup display line already present.")
    sys.exit(0)

# The exact Nyogtha display line (confirmed from diagnostic output)
target = '.$("Variants" -> ("Use " + NyogthaCard.short + " (" + setup.get(UseNyogtha).?("yes").|("no").hl + ")")) ++'
tulzscha_line = '.$("Variants" -> ("Use " + TulzschaCard.short + " (" + setup.get(UseTulzscha).?("yes").|("no").hl + ")")) ++'

if target not in content:
    print("ERROR: Could not find the exact Nyogtha display line.")
    print("Please manually add this line after each occurrence of:")
    print(f'  {target}')
    print("The line to add is:")
    print(f'  {tulzscha_line}')
    sys.exit(1)

# Insert after every occurrence (there are 2 — one for online setup, one for solo setup)
count = content.count(target)
# Replace each occurrence: append the tulzscha line right after
new_content = content.replace(target, target + '\n' + tulzscha_line)

# But we need to preserve indentation — find the indent of the target line
inserted = 0
result = []
i = 0
while i < len(content):
    idx = content.find(target, i)
    if idx == -1:
        result.append(content[i:])
        break
    result.append(content[i:idx + len(target)])
    # Find indentation of this line
    sol = content.rindex('\n', 0, idx) + 1
    indent = ''
    for ch in content[sol:idx]:
        if ch in (' ', '\t'): indent += ch
        else: break
    result.append('\n' + indent + tulzscha_line)
    i = idx + len(target)
    inserted += 1

content = ''.join(result)
print(f"OK: Added Tulzscha setup display line ({inserted} occurrence(s))")

with open(path, "w") as f:
    f.write(content)
print(f"Done: {path} updated.")
