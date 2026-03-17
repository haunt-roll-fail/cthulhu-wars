#!/usr/bin/env python3
"""Script 6: Add Tulzscha img tags to index.html"""
import sys, os, re

sdir = os.environ.get("CWO_SCALA_DIR", "solo")
path = os.path.join(sdir, "index.html")

with open(path, "r") as f:
    content = f.read()

changes = 0

def insert_img_after(content, search_id, new_tag):
    """Find <img ...id="search_id"...> and insert new_tag on next line."""
    pattern = re.compile(r'(<img[^>]+id="' + re.escape(search_id) + r'"[^>]*/>)')
    m = pattern.search(content)
    if not m:
        return content, False
    eol = content.index('\n', m.end())
    sol = content.rindex('\n', 0, m.start()) + 1
    indent = re.match(r'^([ \t]*)', content[sol:]).group(1)
    content = content[:eol+1] + indent + new_tag + '\n' + content[eol+1:]
    return content, True

# Board token
for t in ['n-nyogtha', 'n-daoloth', 'n-abhoth', 'n-byatis']:
    content, ok = insert_img_after(content, t, '<img class="asset" id="n-tulzscha" src="webp/images/n-tulzscha.webp" />')
    if ok:
        print(f"OK: Added board token img after #{t}")
        changes += 1
        break
else:
    print('WARNING: Could not find board token location — add manually:')
    print('<img class="asset" id="n-tulzscha" src="webp/images/n-tulzscha.webp" />')

# Icon
for t in ['nyogtha-icon', 'daoloth-icon', 'abhoth-icon', 'byatis-icon']:
    content, ok = insert_img_after(content, t, '<img class="asset" id="tulzscha-icon" src="webp/info/n-tulzscha.svg" />')
    if ok:
        print(f"OK: Added icon img after #{t}")
        changes += 1
        break
else:
    print('WARNING: Could not find icon location — add manually:')
    print('<img class="asset" id="tulzscha-icon" src="webp/info/n-tulzscha.svg" />')

# Info card
for t in ['info:n-nyogtha', 'info:n-daoloth', 'info:n-abhoth', 'info:n-byatis',
          'info:nyogtha',   'info:daoloth']:
    content, ok = insert_img_after(content, t, '<img class="asset" id="info:n-tulzscha" src="webp/info/n-tulzscha.svg" />')
    if ok:
        print(f"OK: Added info card img after #{t}")
        changes += 1
        break
else:
    print('WARNING: Could not find info card location — add manually:')
    print('<img class="asset" id="info:n-tulzscha" src="webp/info/n-tulzscha.svg" />')

with open(path, "w") as f:
    f.write(content)
print(f"\nDone: {path} updated ({changes} change(s)).")
