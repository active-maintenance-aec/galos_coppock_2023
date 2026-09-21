#!/usr/bin/env python3
"""Find text the published PDFs carry and do not print.

pdftotext reads the content stream and ignores clipping paths, so a label ggplot
drew outside its panel comes back as text no reader can see. PyMuPDF applies the
clip. Every run the first reader emits and the second does not is either a clipped
run or a difference in how the two tokenize, and the two are told apart by looking:
a clipped run is absent from a render of the page, a tokenization difference is not.

`extract_published_claims.R`'s `clipped_runs` table is the confirmed output of this
probe. Re-run after any change to the published PDFs and the two should agree.

    python3 ground_truth/find_clipped_runs.py
"""

import collections
import os
import re
import subprocess

import pymupdf

CATALOG = os.path.expanduser("~/Dropbox/works/catalog/galos_coppock_2023/original_materials")
DOCS = {
    "article": os.path.join(CATALOG, "galos_coppock_2023.pdf"),
    "appendix": os.path.join(CATALOG, "galos_coppock_2023_appendix.pdf"),
}


def poppler_only(path):
    """Tokens carrying a digit that pdftotext emits and a clip-aware reader does not."""
    doc = pymupdf.open(path)
    for page in range(doc.page_count):
        clip_aware = collections.Counter(re.findall(r"\S+", doc[page].get_text()))
        raw = subprocess.run(
            ["pdftotext", "-f", str(page + 1), "-l", str(page + 1), path, "-"],
            capture_output=True, text=True, check=True).stdout
        clip_blind = collections.Counter(re.findall(r"\S+", raw))
        for token, count in (clip_blind - clip_aware).items():
            if re.search(r"\d", token):
                yield page + 1, token, count


if __name__ == "__main__":
    for name, path in DOCS.items():
        print("==", name)
        for page, token, count in poppler_only(path):
            print("   p.%-3d %r x%d" % (page, token, count))
