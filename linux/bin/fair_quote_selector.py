#!/usr/bin/env python3
"""
Option B: Fair quote sampler by flattening quote counts from multiple files.

This script builds a weighted index of quotes across multiple book files.
Each quote gets an equal chance of selection, no matter how many quotes are in its source.

Outputs a file `quote_index.json` with this structure:
[
  {"file": "Book1.txt", "index": 0},
  {"file": "Book1.txt", "index": 1},
  {"file": "Book2.txt", "index": 0},
  ...
]

To retrieve a quote:
1. Load quote_index.json
2. Pick a random entry
3. Load the specified file and extract the nth quote (split by '***')
"""
import os, json, re, pathlib

QUOTE_DIR=os.path.expanduser('~/org/book-notes/shortcut_friendly/')
INDEX_OUT = "quote_index.txt"

index = []

for path in pathlib.Path(QUOTE_DIR).glob("*.txt"):
    with path.open(encoding="utf-8") as f:
        quotes = re.split(r'(?m)^\*\*\*\s*', f.read())
        quotes = [q.strip() for q in quotes if q.strip()]  # remove empty entries
        print(f"{path.name} has {len(quotes)} quotes")
        for i in range(len(quotes)):
            index.append({"file": path.name, "index": i})

with open(f"{QUOTE_DIR}/{INDEX_OUT}", "w", encoding="utf-8") as f:
    json.dump(index, f, indent=2)

print(f"Built index with {len(index)} total quotes across {len(set(d['file'] for d in index))} files.")
