#!/usr/bin/env python3
"""
Master script: Run all Tulzscha IGOO implementation steps.
Run from ANYWHERE — it auto-detects the repo root.

    python3 run_all.py
"""

import subprocess, sys, os

script_dir = os.path.dirname(os.path.abspath(__file__))

def find_repo_root():
    for start in [script_dir, os.path.expanduser("~/cthulhu-wars"), os.path.expanduser("~")]:
        for dirpath, dirnames, filenames in os.walk(start):
            if "IGOOs.scala" in filenames:
                return dirpath, os.path.basename(dirpath)
            dirnames[:] = [d for d in dirnames if not d.startswith('.') and d not in ('node_modules','target')]
    return None, None

igoos_dir, scala_subdir = find_repo_root()
if igoos_dir is None:
    print("ERROR: Could not find IGOOs.scala anywhere. Make sure the script is inside your cthulhu-wars folder.")
    sys.exit(1)

repo_root = os.path.dirname(igoos_dir)
print(f"Repo root:        {repo_root}")
print(f"Scala source dir: {scala_subdir}/")
print()
os.chdir(repo_root)

env = os.environ.copy()
env["CWO_SCALA_DIR"] = scala_subdir
env["CWO_REPO_ROOT"] = repo_root

scripts = [
    ("01_add_tulzscha_igoos.py",              "Add Tulzscha objects to IGOOs.scala"),
    ("02_add_tulzscha_expansion.py",          "Add Tulzscha logic to IGOOsExpansion"),
    ("03_add_tulzscha_game.py",               "Add UseTulzscha to Game.scala"),
    ("04_add_tulzscha_overlay.py",            "Add Tulzscha info card to overlay.scala"),
    ("05_add_tulzscha_solo.py",               "Add Tulzscha to CthulhuWarsSolo.scala"),
    ("06_add_tulzscha_index_html.py",         "Add Tulzscha img tags to index.html"),
    ("07_ceremony_of_annihilation_hook.py",   "Find & guide Ceremony of Annihilation ritual hook"),
]

print("=" * 70)
print("Tulzscha IGOO Implementation — Running all scripts")
print("=" * 70)
print()

all_passed = True
for script, description in scripts:
    script_path = os.path.join(script_dir, script)
    print(f"─── {description}")
    result = subprocess.run([sys.executable, script_path], env=env)
    if result.returncode != 0:
        print(f"    !! FAILED (exit {result.returncode})")
        all_passed = False
    print()

print("=" * 70)
print("All done!" if all_passed else "Finished with errors — review output above.")
print()
print(f"Next: compile with:  cd {repo_root}/{scala_subdir} && sbt fastOptJS")
print(f"Then restart server and test in Incognito mode.")
print("=" * 70)
