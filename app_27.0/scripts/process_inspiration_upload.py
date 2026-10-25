#!/usr/bin/env python3
"""Process one admin upload → WebP with © YNow Team credit."""
import sys
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

CREDIT = "© YNow Team"
MAX_EDGE = 960
WEBP_Q = 75


def font(size: int):
    for p in (
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
    ):
        if Path(p).exists():
            try:
                return ImageFont.truetype(p, size)
            except Exception:
                pass
    return ImageFont.load_default()


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
    draw = ImageDraw.Draw(im)
    f = font(max(11, im.size[0] // 55))
    pad = max(6, im.size[0] // 80)
    bbox = draw.textbbox((0, 0), CREDIT, font=f)
    tw, th = bbox[2] - bbox[0], bbox[3] - bbox[1]
    x = im.size[0] - tw - pad
    y = im.size[1] - th - pad
    draw.rectangle((x - 4, y - 2, x + tw + 4, y + th + 3), fill=(0, 0, 0, 110))
    draw.text((x, y), CREDIT, font=f, fill=(255, 255, 255, 230))
    dest.parent.mkdir(parents=True, exist_ok=True)
    im.save(dest, "WEBP", quality=WEBP_Q, method=6)


if __name__ == "__main__":
    main()
