#!/usr/bin/env python3
"""
Fix the Tulzscha setup display line — replace the bare insertion with the
properly guarded version matching the other IGOO display lines.
"""
import sys, os, re

path = os.path.expanduser("~/cthulhu-wars/solo/CthulhuWarsSolo.scala")

with open(path) as f:
    content = f.read()

# The bad line that was inserted (no guard, wrong position)
bad = '        .$("Variants" -> ("Use " + TulzschaCard.short + " (" + setup.get(UseTulzscha).?("yes").|("no").hl + ")")) ++\n'

# The correct line with the IGOOs guard
good = '    (setup.options.has(IGOOs))\n        .$("Variants" -> ("Use " + TulzschaCard.short + " (" + setup.get(UseTulzscha).?("yes").|("no").hl + ")")) ++\n'

# The Nyogtha line it should follow (with its guard)
nyogtha_with_guard = '    (setup.options.has(IGOOs))\n        .$("Variants" -> ("Use " + NyogthaCard.short + " (" + setup.get(UseNyogtha).?("yes").|("no").hl + ")")) ++'

count_bad = content.count(bad)
print(f"Found {count_bad} bad Tulzscha display line(s)")

# Remove all bad lines first
content = content.replace(bad, '')

# Now insert the correct line after each Nyogtha guarded line
count_nyogtha = content.count(nyogtha_with_guard)
print(f"Found {count_nyogtha} Nyogtha guarded display line(s)")

result = []
remaining = content
while True:
    idx = remaining.find(nyogtha_with_guard)
    if idx == -1:
        result.append(remaining)
        break
    end = idx + len(nyogtha_with_guard)
    result.append(remaining[:end])
    result.append('\n' + good.rstrip('\n'))
    remaining = remaining[end:]

content = ''.join(result)

inserted = content.count(good)
print(f"Inserted {inserted} correct Tulzscha display line(s)")

with open(path, 'w') as f:
    f.write(content)

print(f"Done: {path} updated.")
