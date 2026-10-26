#!/usr/bin/env python3
"""Process Inspiration Gallery sources → small WebP under www/inspiration/.

- Resize max edge 960; quality ~75
- Remove near-white backgrounds to transparency when mostly white
- Do not paint an extra copyright line; each source image already carries its own notice
- Skip phone UI screenshots and third-party product captures (listed EXCLUDE)
- For known Chinese conceptual art, paint English replacements where mapped

Re-run from repo root:
  python3 app_27.0/scripts/process_inspiration_gallery.py

One admin upload (WebP only, no extra copyright):
  python3 app_27.0/scripts/process_inspiration_gallery.py --upload SRC DEST.webp
"""

from __future__ import annotations

import json
import os
import re
import sys
import uuid
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

SRC = Path(
    os.environ.get(
        "YNOW_INSPIRATION_SRC",
        "/cursor/stores/bc-ee52ce88-0754-4bd3-9466-84bb18df73aa/media/inspiration-gallery/src",
    )
)
OUT = Path(
    os.environ.get(
        "YNOW_INSPIRATION_OUT",
        str(Path(__file__).resolve().parents[1] / "www" / "inspiration"),
    )
)
MAX_EDGE = 960
WEBP_Q = 75

# Phone UI / live-app / third-party product captures — not re-translatable art.
EXCLUDE = {
    "00DAB506-0E42-4C69-95F1-80D39D8B9091_L0_001.jpg",
    "0EE25E77-8057-463E-8CB8-C4F0509BB42F_L0_001.jpg",
    "60422E9D-E4B5-4221-82AE-B66C8916E023_L0_001.jpg",
    "87B9F1ED-D5AF-4CEA-A619-C3AC5237FE1A_L0_001.jpg",
    "B26C3DD7-7288-4914-81FF-A69E9A1EDDD5_L0_001.jpg",
    "EC658DB7-02E8-481A-9DEA-FC5EEFCFDB3F_L0_001.jpg",
    "E5FD75D3-726C-4700-9A36-DD2ABF6A8ABD_L0_001.jpg",
    "0D1DB3A2-96BD-465C-AA2B-9A8F2F2D8942_L0_001.jpg",
    "ee0ef6db-35c7-4fda-a07b-9b34bca69a4a.png",
    "0a89ecdc-a842-4341-8baf-bc988efc9a49.jpg",
    "7bbf74d7-799a-4310-b5ef-51a294a9e91a.jpg",
    "d1abafbb-4500-484d-9e45-89d21e745a99.jpg",
    "7d638a3f-2a3a-4f88-b1cd-1c31768262d6.jpg",
    "35eb2473-2595-4a33-8f77-43c1847cebef.jpg",
    "d9966851-eab1-416b-90dc-6ec381b1adec.jpg",
    "9aa58ad6-a114-4ff9-83b5-7ce973e4bc3c.jpg",
    "4f74d6e0-e4d8-4c5c-8eb3-8e940ea5826d.jpg",
}

# Stable ids from source basename stem (first 12 hex/alnum chars).
def stable_id(name: str) -> str:
    stem = Path(name).stem
    m = re.match(r"^([0-9a-fA-F-]{8,})", stem)
    if m:
        return m.group(1).lower().replace("-", "")[:12]
    return uuid.uuid5(uuid.NAMESPACE_URL, name).hex[:12]


def load_rgba(path: Path) -> Image.Image:
    im = Image.open(path)
    if getattr(im, "is_animated", False):
        im.seek(0)
    return im.convert("RGBA")


def maybe_clear_white_bg(im: Image.Image, thresh: int = 248) -> Image.Image:
    """If corners are near-white, punch whiteish pixels to alpha."""
    w, h = im.size
    px = im.load()
    samples = [
        px[2, 2],
        px[w - 3, 2],
        px[2, h - 3],
        px[w - 3, h - 3],
        px[w // 2, 2],
        px[w // 2, h - 3],
    ]
    whiteish = sum(1 for r, g, b, a in samples if r >= thresh and g >= thresh and b >= thresh)
    if whiteish < 4:
        return im
    out = im.copy()
    opx = out.load()
    for y in range(h):
        for x in range(w):
            r, g, b, a = opx[x, y]
            if r >= thresh and g >= thresh and b >= thresh and a > 0:
                opx[x, y] = (r, g, b, 0)
    return out


def resize_max(im: Image.Image, max_edge: int = MAX_EDGE) -> Image.Image:
    w, h = im.size
    m = max(w, h)
    if m <= max_edge:
        return im
    scale = max_edge / m
    return im.resize((max(1, int(w * scale)), max(1, int(h * scale))), Image.Resampling.LANCZOS)


def font(size: int) -> ImageFont.ImageFont:
    for p in (
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
        "/System/Library/Fonts/Helvetica.ttc",
    ):
        if os.path.exists(p):
            try:
                return ImageFont.truetype(p, size)
            except Exception:
                pass
    return ImageFont.load_default()


def paint_english_overlays(name: str, im: Image.Image) -> Image.Image:
    """Best-effort English patches for known Chinese conceptual slides."""
    draw = ImageDraw.Draw(im)
    w, h = im.size
    f = font(max(14, w // 40))
    f_sm = font(max(11, w // 55))

    if name.startswith("c9eea742"):
        # Cover Chinese title; English already present for backbone/base pairs.
        draw.rectangle((w * 0.18, h * 0.02, w * 0.82, h * 0.11), fill=(232, 244, 248, 255))
        draw.text((w * 0.22, h * 0.035), "Reference: DNA double helix model", font=f, fill=(20, 20, 20, 255))
        # Cover Chinese in blue/orange boxes while keeping English lead-ins.
        draw.rectangle((w * 0.55, h * 0.28, w * 0.97, h * 0.40), fill=(20, 60, 120, 255))
        draw.text((w * 0.57, h * 0.30), "Reification: Practice = Ops module", font=f_sm, fill=(255, 255, 255, 255))
        draw.rectangle((w * 0.28, h * 0.72, w * 0.78, h * 0.86), fill=(200, 90, 40, 255))
        draw.text((w * 0.30, h * 0.75), "Participation: Activity = Learning module", font=f_sm, fill=(255, 255, 255, 255))
        return im

    if name.startswith("f74c6ef0"):
        # GIF already bilingual; cover Chinese-only axis/legend leftovers.
        # Cover bottom-left Chinese legend with English.
        draw.rectangle((0, h * 0.88, w * 0.42, h), fill=(255, 200, 120, 255))
        draw.text((8, h * 0.90), "X carrier · Y knowledge", font=f_sm, fill=(20, 20, 20, 255))
        return im

    if name.startswith("aa30a1b1") or name.startswith("567fae65"):
        # Supply-chain labels: blanket English title strip.
        draw.rectangle((0, 0, w, max(28, h // 14)), fill=(255, 255, 255, 230))
        draw.text((10, 6), "Supply chain: suppliers · manufacturing · brand · market", font=f_sm, fill=(20, 20, 20, 255))
        return im

    if name.startswith("99e9f1ae"):
        draw.rectangle((0, h * 0.55, w * 0.45, h * 0.72), fill=(255, 255, 255, 230))
        draw.text((10, h * 0.58), "Crowd wisdom", font=f, fill=(20, 20, 20, 255))
        draw.rectangle((w * 0.55, h * 0.55, w, h * 0.72), fill=(255, 255, 255, 230))
        draw.text((w * 0.57, h * 0.58), "Internal R&D talent", font=f, fill=(20, 20, 20, 255))
        return im

    return im


def process_one(path: Path) -> dict | None:
    name = path.name
    if name in EXCLUDE:
        return None
    im = load_rgba(path)
    im = paint_english_overlays(name, im)
    im = maybe_clear_white_bg(im)
    im = resize_max(im)
    gid = stable_id(name)
    out_name = f"{gid}.webp"
    out_path = OUT / out_name
    im.save(out_path, "WEBP", quality=WEBP_Q, method=6)
    return {
        "id": gid,
        "src": f"inspiration/{out_name}",
        "enabled": True,
        "source": "bundled",
        "bytes": out_path.stat().st_size,
        "from": name,
    }


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    for old in OUT.glob("*.webp"):
        old.unlink()
    if not SRC.is_dir():
        raise SystemExit(f"missing source dir: {SRC}")
    items = []
    skipped = []
    for path in sorted(SRC.iterdir()):
        if not path.is_file():
            continue
        if path.suffix.lower() not in {".jpg", ".jpeg", ".png", ".gif", ".webp"}:
            continue
        if path.name in EXCLUDE:
            skipped.append(path.name)
            continue
        row = process_one(path)
        if row:
            items.append(row)
            print(f"ok {path.name} -> {row['src']} ({row['bytes']} B)")
    catalog = {
        "version": 1,
        "frequency": {"show_probability": 1.0, "rotation_interval_sec": 0},
        "images": [
            {k: v for k, v in it.items() if k != "from"} for it in items
        ],
    }
    # rsconnect ignores every file named manifest.json, in any directory.
    (OUT / "gallery_catalog.json").write_text(
        json.dumps(catalog, ensure_ascii=False, indent=2) + "\n", encoding="utf-8"
    )
    legacy = OUT / "manifest.json"
    if legacy.exists():
        legacy.unlink()
    meta = {"bundled": items, "excluded": skipped}
    (OUT / "process_meta.json").write_text(
        json.dumps(meta, ensure_ascii=False, indent=2) + "\n", encoding="utf-8"
    )
    print(f"wrote {len(items)} images; excluded {len(skipped)}")


def process_upload(src: Path, dest: Path) -> int:
    """Resize and write WebP. Does not paint an extra copyright line. Returns byte size."""
    im = load_rgba(src)
    im = maybe_clear_white_bg(im)
    im = resize_max(im)
    dest.parent.mkdir(parents=True, exist_ok=True)
    im.save(dest, "WEBP", quality=WEBP_Q, method=6)
    return dest.stat().st_size


if __name__ == "__main__":
    if len(sys.argv) >= 2 and sys.argv[1] == "--upload":
        if len(sys.argv) != 4:
            raise SystemExit("usage: process_inspiration_gallery.py --upload SRC DEST")
        print(process_upload(Path(sys.argv[2]), Path(sys.argv[3])))
    else:
        main()
