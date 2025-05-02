#!/usr/bin/env python3
"""
Export every top‑ or second‑level heading in a provided .org file into
<Title>_Notebook.org files containing ***‑separated quote blocks.

Enhancements:
* Skips empty quote blocks
* Skips titles if all quotes are empty
* Accepts input file path as argument
* Adds title as org heading
* Adds metadata header with export date
* Marks each quote with a level‑2 heading "** Quote"
"""
import os, re, pathlib, textwrap, sys
from datetime import datetime

if len(sys.argv) < 2:
    print("Usage: python extract_quotes_from_books_orgfile.py <books.org>")
    sys.exit(1)

SOURCE = sys.argv[1]          # your collector file from command line
DEST   = os.path.expanduser("~/books/notes/exported_book_notes")     # folder to write notebooks
os.makedirs(DEST, exist_ok=True)

with open(SOURCE, encoding="utf-8") as fh:
    org = fh.read()

# ── 1. find every level‑1 or level‑2 heading that looks like "* Title" ──
heading_re = re.compile(r'(?m)^(\*{1,2})\s+(?:[A-Z_]+\s+)?(.+?)\s*$')
headings = list(heading_re.finditer(org))

for idx, h in enumerate(headings):
    raw_title = h.group(2)
    # Remove any leading TODO keyword (e.g., DONE, TO_READ)
    title_cleaned = re.sub(r'^[A-Z_]+\s+', '', raw_title)
    # Remove trailing Org-mode tags like ":tag1:tag2:"
    title_cleaned = re.sub(r'\s+:.*?:\s*$', '', title_cleaned).strip()
    body_start = h.end()
    body_end   = headings[idx + 1].start() if idx + 1 < len(headings) else len(org)
    body       = org[body_start:body_end]

    # ── 2. collect quote / verse blocks ──
    raw_blocks = re.findall(r'(?ims)^#\+begin_(?:quote|verse)\b([\s\S]*?)^#\+end_\w+', body)

    # strip & drop empty blocks
    quotes = [textwrap.dedent(b).strip() for b in raw_blocks if b.strip()]
    if not quotes:
        continue  # no non‑empty quotes under this heading

    # ── 3. normalise title -> filename ──
    safe_title = re.sub(r'[^A-Za-z0-9 _-]', '', title_cleaned).strip()

    # file_name_like_this
    filename_base = re.sub(r'[^a-z0-9]+', '_', safe_title.lower().strip())
    fname = pathlib.Path(DEST, f"{filename_base}_notebook.org")

    with fname.open('w', encoding='utf-8') as out:
        out.write(f"* {title_cleaned}\n")
        out.write(f"\n#+EXPORT_INFO: Hand-typed quotes originally from {SOURCE}. Extracted with extract_quotes_from_books_orgfile.py on {datetime.today().strftime('%Y-%m-%d')}\n\n")
        for q in quotes:
            out.write("*** Highlight\n\n")
            out.write(f"{q}\n\n")
    print(f"{fname.name:<40} ← {len(quotes)} quote(s)")

print("\nDone.")


    
