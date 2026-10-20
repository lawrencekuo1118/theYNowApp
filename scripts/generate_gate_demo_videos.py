#!/usr/bin/env python3
"""Generate ~10s Sign-in-gate demo clips for each locked feature.

Writes mp4 + webm under app_25.0/www/demos/.
Source of feature list: ynow_gate_features() (lab, checklist, alerts, account,
pdf, snap_defaults, inner_lock, atx).

Usage (repo root):
  python3 scripts/generate_gate_demo_videos.py
"""
from __future__ import annotations

import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "app_25.0" / "www" / "demos"
W, H, FPS = 960, 540, 30
DUR = 10.0  # seconds — within the ≤10s gate-demo budget (looping on the page)

# en-US copy aligned with auth_gate_* strings / captions (UI locale still switches).
FEATURES = {
    "lab": {
        "title": "Quantitative Backtest Lab",
        "kicker": "APP member exclusive",
        "bullets": [
            "HFV and Quant Backtest worksheets",
            "Fair Value replay and Lab reports",
            "FYN spend for Lab runs when points are on",
        ],
        "caption": "Lab demo — unlock after sign-in",
    },
    "checklist": {
        "title": "Decision Checklist",
        "kicker": "APP member exclusive",
        "bullets": [
            "Mandatory Decision SOP gates",
            "Bear/Base MOS, F-Score, quality checks",
            "YNOW verdict unlock after SOP gates pass",
        ],
        "caption": "Checklist demo — Decision SOP flow",
    },
    "alerts": {
        "title": "Value Alerts",
        "kicker": "APP member exclusive",
        "bullets": [
            "Save and manage value / price alert rules",
            "Email notify to your account mailbox",
            "Alert history and FYN alert_slot spend",
        ],
        "caption": "Value Alerts demo — rules and notify",
    },
    "account": {
        "title": "Account workspace",
        "kicker": "Sign-in required",
        "bullets": [
            "Account overview and page themes",
            "FYN / PYN balances and daily check-in",
            "Complete Points ledger history",
        ],
        "caption": "Account demo — overview and points",
    },
    "pdf": {
        "title": "Valuation PDF download",
        "kicker": "Sign-in required",
        "bullets": [
            "Broker-style report for the active ticker",
            "Three free downloads each Asia/Taipei day",
            "Extra downloads cost 10 FYN",
        ],
        "caption": "PDF demo — report download",
    },
    "snap_defaults": {
        "title": "System defaults (APP_DEFAULTS)",
        "kicker": "System admin unlock",
        "bullets": [
            "View and edit live-line APP_DEFAULTS",
            "Download / restore defaults CSV while unlocked",
            "Locks again when you exit System admin",
        ],
        "caption": "APP_DEFAULTS demo — sysadmin unlock",
    },
    "inner_lock": {
        "title": "Locked tab",
        "kicker": "Sign-in / admin unlock",
        "bullets": [
            "Lock icon replaces the tab title while restricted",
            "Sign in, then ask a system admin to clear the lock",
            "Other tabs on the page stay available",
        ],
        "caption": "Locked tab demo",
    },
    "atx": {
        "title": "Asset Transmission Root",
        "kicker": "Full mode — unlock required",
        "bullets": [
            "Macro drivers into rate, dollar, liquidity hubs",
            "Asset classes: bonds, equities, metals, FX, crypto",
            "Regime card is surveillance only — not a trade signal",
        ],
        "caption": "Asset Transmission Root demo — map after unlock",
    },
}

BG = (26, 26, 26)
FG = (255, 255, 255)
MUTED = (180, 180, 180)
ACCENT = (36, 156, 96)  # logo green
CARD = (45, 45, 45)


def _font(size: int, bold: bool = False):
    from PIL import ImageFont

    candidates = [
        "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf" if bold else "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf" if bold else "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
    ]
    for p in candidates:
        if Path(p).is_file():
            return ImageFont.truetype(p, size=size)
    return ImageFont.load_default()


def _wrap(draw, text: str, font, max_w: int) -> list[str]:
    words = text.split()
    lines, cur = [], ""
    for w in words:
        trial = (cur + " " + w).strip()
        if draw.textlength(trial, font=font) <= max_w:
            cur = trial
        else:
            if cur:
                lines.append(cur)
            cur = w
    if cur:
        lines.append(cur)
    return lines or [text]


def render_frame(feature: str, t: float):
    from PIL import Image, ImageDraw

    meta = FEATURES[feature]
    img = Image.new("RGB", (W, H), BG)
    draw = ImageDraw.Draw(img)
    title_f = _font(36, bold=True)
    kick_f = _font(18, bold=True)
    body_f = _font(22)
    cap_f = _font(18)
    cta_f = _font(24, bold=True)

    # Top accent bar
    draw.rectangle([0, 0, W, 6], fill=ACCENT)

    # Scene timing
    # 0–3s title, 3–7.5s bullets, 7.5–10s CTA
    if t < 3.0:
        alpha_phase = "title"
    elif t < 7.5:
        alpha_phase = "bullets"
    else:
        alpha_phase = "cta"

    # Title block (always visible, fades emphasis)
    y = 70
    for line in _wrap(draw, meta["title"], title_f, W - 120):
        tw = draw.textlength(line, font=title_f)
        draw.text(((W - tw) / 2, y), line, font=title_f, fill=FG)
        y += 46
    kick = meta["kicker"]
    kw = draw.textlength(kick, font=kick_f)
    draw.text(((W - kw) / 2, y + 8), kick, font=kick_f, fill=ACCENT)

    if alpha_phase == "bullets":
        # Progress through bullets
        elapsed = t - 3.0
        n_show = 1 + int(elapsed // 1.4)
        box_top = 230
        draw.rounded_rectangle([60, box_top, W - 60, H - 90], radius=12, fill=CARD)
        by = box_top + 28
        for i, b in enumerate(meta["bullets"][: max(1, min(3, n_show))]):
            bullet = f"•  {b}"
            for line in _wrap(draw, bullet, body_f, W - 160):
                draw.text((90, by), line, font=body_f, fill=FG)
                by += 32
            by += 10

    if alpha_phase == "cta":
        draw.rounded_rectangle([60, 240, W - 60, H - 90], radius=12, fill=CARD)
        cap = meta["caption"]
        cy = 280
        for line in _wrap(draw, cap, cap_f, W - 160):
            tw = draw.textlength(line, font=cap_f)
            draw.text(((W - tw) / 2, cy), line, font=cap_f, fill=MUTED)
            cy += 28
        cta = "Sign in to unlock"
        tw = draw.textlength(cta, font=cta_f)
        # CTA pill
        pad_x, pad_y = 28, 14
        bx0 = (W - tw) / 2 - pad_x
        by0 = cy + 24
        bx1 = (W + tw) / 2 + pad_x
        by1 = by0 + 40
        draw.rounded_rectangle([bx0, by0, bx1, by1], radius=6, fill=ACCENT)
        draw.text(((W - tw) / 2, by0 + pad_y - 4), cta, font=cta_f, fill=FG)

    # Bottom brand
    brand = "YNow · Full mode"
    bw = draw.textlength(brand, font=cap_f)
    draw.text(((W - bw) / 2, H - 42), brand, font=cap_f, fill=MUTED)
    return img


def write_feature(feature: str, tmp: Path) -> None:
    frames_dir = tmp / feature
    frames_dir.mkdir(parents=True, exist_ok=True)
    n = int(DUR * FPS)
    for i in range(n):
        t = i / FPS
        render_frame(feature, t).save(frames_dir / f"f{i:04d}.png")

    pattern = str(frames_dir / "f%04d.png")
    mp4 = OUT / f"{feature}.mp4"
    webm = OUT / f"{feature}.webm"

    subprocess.check_call(
        [
            "ffmpeg", "-y", "-hide_banner", "-loglevel", "error",
            "-framerate", str(FPS), "-i", pattern,
            "-c:v", "libx264", "-pix_fmt", "yuv420p", "-profile:v", "main",
            "-movflags", "+faststart", "-crf", "23",
            str(mp4),
        ]
    )
    subprocess.check_call(
        [
            "ffmpeg", "-y", "-hide_banner", "-loglevel", "error",
            "-framerate", str(FPS), "-i", pattern,
            "-c:v", "libvpx-vp9", "-b:v", "0", "-crf", "32", "-pix_fmt", "yuv420p",
            str(webm),
        ]
    )
    print(f"wrote {mp4.name} + {webm.name}")


def main() -> None:
    if not shutil.which("ffmpeg"):
        raise SystemExit("ffmpeg required")
    OUT.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="ynow-gate-demos-") as td:
        tmp = Path(td)
        for feat in FEATURES:
            write_feature(feat, tmp)
    print("done →", OUT)


if __name__ == "__main__":
    main()
