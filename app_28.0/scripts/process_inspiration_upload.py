#!/usr/bin/env python3
"""Process one admin upload → WebP.

Each source image already carries its own copyright notice. Do not paint
another copyright line onto the bitmap.
"""
import sys
from pathlib import Path

from PIL import Image

MAX_EDGE = 960
WEBP_Q = 75


def main() -> None:
    if len(sys.argv) < 3:
        raise SystemExit("usage: process_inspiration_upload.py SRC DEST.webp")
    src, dest = Path(sys.argv[1]), Path(sys.argv[2])
    im = Image.open(src)
    if getattr(im, "is_animated", False):
        im.seek(0)
    im = im.convert("RGBA")
    w, h = im.size
    m = max(w, h)
    if m > MAX_EDGE:
        scale = MAX_EDGE / m
        im = im.resize((max(1, int(w * scale)), max(1, int(h * scale))), Image.Resampling.LANCZOS)
    dest.parent.mkdir(parents=True, exist_ok=True)
    im.save(dest, "WEBP", quality=WEBP_Q, method=6)


if __name__ == "__main__":
    main()
