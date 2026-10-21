#!/usr/bin/env python3
"""Generate ~10s Sign-in-gate demo clips with real page scroll (not text cards).

Default mode (recommended):
  Build tall AdminLTE-style fixtures under scripts/gate_demo_pages/ and
  Playwright-scroll each page while recording → app_25.0/www/demos/<id>.{mp4,webm}.

Optional live mode:
  YNOW_DEMO_CAPTURE=1 on a local Shiny process, then:
    YNOW_DEMO_MODE=live python3 scripts/generate_gate_demo_videos.py

Usage (repo root):
  .venv-demos/bin/python scripts/generate_gate_demo_videos.py
"""
from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "app_25.0" / "www" / "demos"
PAGES = ROOT / "scripts" / "gate_demo_pages"
W, H, FPS = 960, 540, 24
DUR = 10.0
MODE = os.environ.get("YNOW_DEMO_MODE", "fixture").strip().lower()  # fixture | live
BASE_URL = os.environ.get("YNOW_DEMO_BASE_URL", "http://127.0.0.1:3840/")

FEATURES = {
    "lab": {"tab": "lab_notes", "wait_ms": 2500},
    "checklist": {"tab": "decision_checklist", "wait_ms": 2500},
    "alerts": {"tab": "price_alerts", "wait_ms": 2500},
    "account": {"tab": "account_admin", "wait_ms": 2500},
    "pdf": {"tab": "dashboard", "wait_ms": 2000},
    "snap_defaults": {"tab": "snapshot", "wait_ms": 2500},
    "inner_lock": {"tab": "hfv", "wait_ms": 2500},
    "atx": {"tab": "asset_transmission", "wait_ms": 3000},
}


def _encode(pattern: str, mp4: Path, webm: Path) -> None:
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


def _scroll_info(page):
    return page.evaluate(
        """() => {
          const el = document.querySelector('#scroll-root')
            || document.querySelector('.content-wrapper')
            || document.querySelector('.content')
            || document.scrollingElement
            || document.documentElement;
          const max = Math.max(0, (el.scrollHeight || 0) - (el.clientHeight || window.innerHeight));
          return { max, id: el.id || el.className || el.tagName };
        }"""
    )


def _scroll_to(page, y: float) -> None:
    page.evaluate(
        """(y) => {
          const el = document.querySelector('#scroll-root')
            || document.querySelector('.content-wrapper')
            || document.querySelector('.content')
            || document.scrollingElement
            || document.documentElement;
          if (el && el.scrollTo) el.scrollTo({ top: y, behavior: 'instant' });
          else window.scrollTo(0, y);
        }""",
        y,
    )


def _record_scroll(page, frames_dir: Path) -> None:
    frames_dir.mkdir(parents=True, exist_ok=True)
    info = _scroll_info(page)
    max_y = float(info.get("max") or 0)
    if max_y < 200:
        page.evaluate(
            """() => {
              const pad = document.createElement('div');
              pad.style.height = '1600px';
              pad.style.opacity = '0';
              (document.querySelector('#scroll-root') || document.body).appendChild(pad);
            }"""
        )
        max_y = max(float(_scroll_info(page).get("max") or 0), 1000.0)

    n = int(DUR * FPS)
    for i in range(n):
        t = i / max(n - 1, 1)
        u = t * t * (3 - 2 * t)
        _scroll_to(page, max_y * u)
        page.wait_for_timeout(int(1000 / FPS))
        page.screenshot(path=str(frames_dir / f"f{i:04d}.png"), type="png")


def capture_fixture(page, feature: str, frames_dir: Path) -> None:
    html = (PAGES / f"{feature}.html").resolve()
    if not html.is_file():
        raise FileNotFoundError(f"missing fixture {html} — run scripts/gate_demo_pages/build_pages.py")
    page.goto(html.as_uri(), wait_until="domcontentloaded", timeout=30000)
    page.wait_for_timeout(400)
    _record_scroll(page, frames_dir)


def _open_tab(page, tab: str) -> None:
    page.evaluate(
        """(t) => {
          try { document.body.classList.remove('sidebar-collapse'); } catch (e) {}
          const a = document.querySelector(".sidebar-menu a[data-value='" + t + "']");
          if (a) { try { a.scrollIntoView({block:'center'}); a.click(); } catch (e2) {} }
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('sidebar_tabs', t, {priority: 'event'});
          }
        }""",
        tab,
    )


def capture_live(page, feature: str, frames_dir: Path) -> None:
    meta = FEATURES[feature]
    page.goto(BASE_URL, wait_until="domcontentloaded", timeout=90000)
    page.wait_for_timeout(3500)
    page.evaluate("() => { try { document.body.classList.remove('sidebar-collapse'); } catch (e) {} }")
    try:
        page.locator("button:has-text('Accept'), button:has-text('同意')").first.click(timeout=1200)
    except Exception:
        pass
    for _ in range(24):
        if page.evaluate("() => document.body.classList.contains('ynow-authed')"):
            break
        page.wait_for_timeout(250)
    _open_tab(page, meta["tab"])
    page.wait_for_timeout(int(meta.get("wait_ms", 2000)))
    _record_scroll(page, frames_dir)


def main() -> None:
    if not shutil.which("ffmpeg"):
        raise SystemExit("ffmpeg required")
    try:
        from playwright.sync_api import sync_playwright
    except ImportError as e:
        raise SystemExit(
            "playwright required — .venv-demos/bin/python or: pip install playwright && playwright install chromium"
        ) from e

    # Ensure fixtures exist for fixture mode
    if MODE != "live":
        subprocess.check_call(
            [os.environ.get("PYTHON", "python3"), str(PAGES / "build_pages.py")],
            cwd=str(ROOT),
        )

    OUT.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="ynow-gate-demos-") as td:
        tmp = Path(td)
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True)
            page = browser.new_context(
                viewport={"width": W, "height": H},
                device_scale_factor=1,
            ).new_page()
            for feat in FEATURES:
                print(f"capturing {feat} ({MODE}) …")
                t0 = time.time()
                frames = tmp / feat
                if MODE == "live":
                    capture_live(page, feat, frames)
                else:
                    capture_fixture(page, feat, frames)
                pattern = str(frames / "f%04d.png")
                _encode(pattern, OUT / f"{feat}.mp4", OUT / f"{feat}.webm")
                print(f"  wrote {feat}.mp4 + {feat}.webm ({time.time() - t0:.1f}s)")
            browser.close()
    print("done →", OUT, f"(mode={MODE})")


if __name__ == "__main__":
    main()
