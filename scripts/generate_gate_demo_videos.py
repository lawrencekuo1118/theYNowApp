#!/usr/bin/env python3
"""Capture ~10s Sign-in-gate demo clips from the running App UI only.

Product-facing locked tabs/features only (lab, checklist, alerts, pdf, atx).
Non-product / guest-hidden / service ids (account, snap_defaults, inner_lock)
are hide-vs-show only — no Sign-in promo page and no teaser; do not add them.

Each clip is a Playwright scroll of the live Shiny App, then ffmpeg writes
app_26.0/www/demos/<id>.mp4 and .webm. There is no fixture HTML path.

  YNOW_DEMO_BASE_URL=http://127.0.0.1:3840/ .venv-demos/bin/python scripts/generate_gate_demo_videos.py
"""
from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import time
from pathlib import Path
from typing import TypedDict

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "app_26.0" / "www" / "demos"
W, H, FPS = 960, 540, 24
DUR = 10.0
AUTH_WAIT_MS = 15_000
BASE_URL = os.environ.get("YNOW_DEMO_BASE_URL", "http://127.0.0.1:3840/")


class CaptureTarget(TypedDict):
    tab: str
    wait_ms: int


# Keep in sync with ynow_gate_demo_features() in app_26.0/auth_account.R
FEATURES: dict[str, CaptureTarget] = {
    "lab": {"tab": "lab_notes", "wait_ms": 4500},
    "checklist": {"tab": "decision_checklist", "wait_ms": 4500},
    "alerts": {"tab": "price_alerts", "wait_ms": 3500},
    "pdf": {"tab": "dashboard", "wait_ms": 3500},
    "atx": {"tab": "asset_transmission", "wait_ms": 4500},
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


def _require_live_mode() -> None:
    mode = os.environ.get("YNOW_DEMO_MODE", "live").strip().lower()
    if mode != "live":
        raise SystemExit(
            "Demos are captured from the running App UI only "
            f"(YNOW_DEMO_MODE={mode!r} is refused)."
        )


def _require_authed(page) -> None:
    try:
        page.wait_for_function(
            "() => document.body && document.body.classList.contains('ynow-authed')",
            timeout=AUTH_WAIT_MS,
        )
    except Exception as exc:
        raise SystemExit(
            "body.ynow-authed did not appear within 15s. "
            "Demos are captured from the running App UI only."
        ) from exc


def _dismiss_consent(page) -> None:
    try:
        page.locator("#ynow_cookie_accept_all").first.click(timeout=1500)
        return
    except Exception:
        pass
    for name in ("Accept all", "全部允許", "Accept", "同意"):
        try:
            page.get_by_role("button", name=name).first.click(timeout=800)
            return
        except Exception:
            continue


def _ensure_sidebar_open(page) -> None:
    page.evaluate(
        """() => {
          if (window.ynowEnsureSidebarOpen) window.ynowEnsureSidebarOpen();
          document.body.classList.remove('sidebar-collapse');
        }"""
    )
    try:
        page.wait_for_function(
            """() => {
              if (document.body.classList.contains('sidebar-collapse')) return false;
              const s = document.querySelector('.main-sidebar');
              if (!s) return true;
              const b = s.getBoundingClientRect();
              return b.width > 40;
            }""",
            timeout=5000,
        )
    except Exception:
        page.evaluate("() => document.body.classList.remove('sidebar-collapse')")


def _scroll_link_into_sidebar(page, selector: str) -> None:
    # The sidebar is position:fixed and taller than the window. A link can be
    # "visible" in CSS and still sit outside the viewport, so Playwright will not click it.
    found = page.evaluate(
        """(sel) => {
          const a = document.querySelector(sel);
          if (!a) return false;
          let node = a.parentElement;
          while (node && node !== document.documentElement) {
            const s = getComputedStyle(node);
            if (/(auto|scroll)/.test(s.overflowY) && node.scrollHeight > node.clientHeight + 4) {
              const top = a.getBoundingClientRect().top - node.getBoundingClientRect().top + node.scrollTop;
              node.scrollTop = Math.max(0, top - node.clientHeight / 2);
              return true;
            }
            node = node.parentElement;
          }
          try { a.scrollIntoView({block: 'center'}); } catch (e) {}
          return true;
        }""",
        selector,
    )
    if not found:
        raise SystemExit(f"missing {selector}")


def _require_active(page, tab: str) -> None:
    ok = page.evaluate(
        """(tab) => {
          const nodes = document.querySelectorAll("a[data-value='" + tab + "']");
          for (const a of nodes) {
            const li = a.closest('li');
            if (li && li.classList.contains('active')) return true;
            const href = a.getAttribute('href');
            const pane = href ? document.querySelector(href) : null;
            if (pane && pane.classList.contains('active')) return true;
          }
          return false;
        }""",
        tab,
    )
    if not ok:
        raise SystemExit(f"{tab} did not become the active tab")


def _open_sidebar(page, tab: str) -> None:
    _ensure_sidebar_open(page)
    page.evaluate(
        """(tab) => {
          const a = document.querySelector(".sidebar-menu a[data-value='" + tab + "']");
          if (!a) return;
          const tree = a.closest('li.treeview');
          if (tree && !tree.classList.contains('active') && !tree.classList.contains('menu-open')) {
            const parent = tree.querySelector(':scope > a');
            if (parent) parent.click();
          }
        }""",
        tab,
    )
    page.wait_for_timeout(250)
    sel = f".sidebar-menu a[data-value='{tab}']"
    _scroll_link_into_sidebar(page, sel)
    page.locator(sel).first.click(timeout=8000)
    _require_active(page, tab)


def _open_target(page, target: CaptureTarget) -> None:
    _open_sidebar(page, target["tab"])


def _scroll_state(page) -> dict:
    # .content-wrapper is overflow:visible and grows the document. Scrolling
    # that element does not move the page. The window does, under the fixed header.
    return page.evaluate(
        """() => {
          const doc = document.scrollingElement || document.documentElement;
          const docMax = Math.max(0, doc.scrollHeight - window.innerHeight);
          let elMax = 0;
          let sel = '';
          for (const candidate of ['.content-wrapper', '.content']) {
            const el = document.querySelector(candidate);
            if (!el) continue;
            const style = getComputedStyle(el);
            const max = Math.max(0, el.scrollHeight - el.clientHeight);
            if (/(auto|scroll)/.test(style.overflowY) && max > elMax) {
              elMax = max;
              sel = candidate;
            }
          }
          if (elMax > docMax + 20) return {mode: 'el', sel, max: elMax};
          return {mode: 'window', sel: '', max: docMax};
        }"""
    )


def _scroll_to(page, state: dict, y: float) -> None:
    if state.get("mode") == "el":
        page.evaluate(
            """([sel, y]) => {
              const el = document.querySelector(sel);
              if (el && el.scrollTo) el.scrollTo({top: y, behavior: 'instant'});
            }""",
            [state["sel"], y],
        )
        return
    page.evaluate("(y) => window.scrollTo(0, y)", y)


def _record_scroll(page, frames_dir: Path) -> None:
    frames_dir.mkdir(parents=True, exist_ok=True)
    state = _scroll_state(page)
    max_y = float(state.get("max") or 0)
    _scroll_to(page, state, 0)
    n = int(DUR * FPS)
    for i in range(n):
        t = i / max(n - 1, 1)
        u = t * t * (3 - 2 * t)
        _scroll_to(page, state, max_y * u)
        page.wait_for_timeout(int(1000 / FPS))
        page.screenshot(path=str(frames_dir / f"f{i:04d}.png"), type="png")


def capture_live(page, feature: str, frames_dir: Path) -> None:
    target = FEATURES[feature]
    _open_target(page, target)
    page.wait_for_timeout(int(target["wait_ms"]))
    _record_scroll(page, frames_dir)


def main() -> None:
    _require_live_mode()
    if not shutil.which("ffmpeg"):
        raise SystemExit("ffmpeg required")
    try:
        from playwright.sync_api import sync_playwright
    except ImportError as exc:
        raise SystemExit(
            "playwright required. Use .venv-demos/bin/python "
            "or pip install playwright && playwright install chromium"
        ) from exc

    OUT.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="ynow-gate-demos-") as td:
        tmp = Path(td)
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True)
            page = browser.new_context(
                viewport={"width": W, "height": H},
                device_scale_factor=1,
            ).new_page()
            page.goto(BASE_URL, wait_until="domcontentloaded", timeout=90000)
            _require_authed(page)
            _dismiss_consent(page)
            for feat in FEATURES:
                print(f"capturing {feat} (live) …", flush=True)
                t0 = time.time()
                frames = tmp / feat
                capture_live(page, feat, frames)
                pattern = str(frames / "f%04d.png")
                _encode(pattern, OUT / f"{feat}.mp4", OUT / f"{feat}.webm")
                print(f"  wrote {feat}.mp4 + {feat}.webm ({time.time() - t0:.1f}s)", flush=True)
            browser.close()
    print("done →", OUT, "(live App UI)")


if __name__ == "__main__":
    main()
