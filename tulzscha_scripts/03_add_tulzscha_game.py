#!/usr/bin/env python3
"""Script 3: Add UseTulzscha to Game.scala"""
import sys, os, re

sdir = os.environ.get("CWO_SCALA_DIR", "solo")
path = os.path.join(sdir, "Game.scala")

with open(path, "r") as f:
    content = f.read()

# 1. Add UseTulzscha game option after last IGOO option
inserted = False
for marker in ['UseNyogtha', 'UseDaoloth', 'UseAbhoth', 'UseByatis']:
    pattern = re.compile(r'(case object ' + marker + r'[^\n]+)')
    m = pattern.search(content)
    if m:
        insert_pos = m.end()
        content = content[:insert_pos] + '\ncase object UseTulzscha extends LoyaltyCardGameOption(TulzschaCard)' + content[insert_pos:]
        print(f"OK: Added UseTulzscha after {marker}")
        inserted = True
        break

if not inserted:
    print("ERROR: Could not find any IGOO LoyaltyCardGameOption in Game.scala")
    print("Please manually add: case object UseTulzscha extends LoyaltyCardGameOption(TulzschaCard)")
    sys.exit(1)

# 2. Register in GameOptions.all
registered = False
for opt in ['UseNyogtha,', 'UseDaoloth,', 'UseAbhoth,', 'UseByatis,']:
    if opt in content:
        idx = content.index(opt)
        eol = content.index('\n', idx)
        # grab indentation of that line
        sol = content.rindex('\n', 0, idx) + 1
        indent = re.match(r'^([ \t]*)', content[sol:]).group(1)
        content = content[:eol+1] + indent + 'UseTulzscha,\n' + content[eol+1:]
        print(f"OK: Registered UseTulzscha in GameOptions.all after {opt}")
        registered = True
        break

if not registered:
    print("WARNING: Could not auto-register UseTulzscha in GameOptions.all")
    print("Please manually add 'UseTulzscha,' to the GameOptions.all list")

with open(path, "w") as f:
    f.write(content)
print(f"\nDone: {path} updated.")
