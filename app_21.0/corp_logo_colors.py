# -*- coding: utf-8 -*-
"""Extract up to two primary brand colors from a company's official logo.

Used after Search to tint the company full-name heading (``.ynow-corpname``).
Sources (in order): Financial Modeling Prep stock logo, Parqet PNG, DuckDuckGo
site icon / Google favicon from Yahoo ``website``.
"""

from __future__ import annotations

import io
import math
import re
from collections import Counter
from typing import Any, Dict, List, Optional, Sequence, Tuple
from urllib.parse import urlparse

import requests

try:
    from PIL import Image
except Exception:  # pragma: no cover
    Image = None  # type: ignore

_UA = {
    "User-Agent": (
        "Mozilla/5.0 (compatible; theYNowApp/21.0; +https://github.com/lawrencekuo1118/theYNowApp)"
    )
}
_HEX_RE = re.compile(r"^#[0-9A-Fa-f]{6}$")


def _sat(r: int, g: int, b: int) -> float:
    mx, mn = max(r, g, b), min(r, g, b)
    if mx <= 0:
        return 0.0
    return (mx - mn) / float(mx)


def _dist(a: Sequence[int], b: Sequence[int]) -> float:
    return math.sqrt(sum((int(x) - int(y)) ** 2 for x, y in zip(a, b)))


def _rel_luminance(r: int, g: int, b: int) -> float:
    def _chan(c: int) -> float:
        x = c / 255.0
        return x / 12.92 if x <= 0.03928 else ((x + 0.055) / 1.055) ** 2.4

    return 0.2126 * _chan(r) + 0.7152 * _chan(g) + 0.0722 * _chan(b)


def _ok_for_light_text(r: int, g: int, b: int) -> bool:
    """Keep colors readable as text on the app's light background."""
    # Contrast vs white ≈ (1.05) / (L+0.05); require ≥ ~3:1 → L ≲ 0.55
    # Also drop near-white / pastel washes that wash out on #fff.
    L = _rel_luminance(r, g, b)
    if L > 0.58:
        return False
    if max(r, g, b) > 240 and _sat(r, g, b) < 0.25:
        return False
    return True


def _domain_from_website(website: Optional[str]) -> str:
    w = str(website or "").strip()
    if not w:
        return ""
    if "://" not in w:
        w = "https://" + w
    try:
        host = urlparse(w).netloc or ""
    except Exception:
        return ""
    host = host.lower().strip()
    if host.startswith("www."):
        host = host[4:]
    return host


def _ticker_symbol(ticker: str) -> str:
    t = str(ticker or "").strip().upper()
    if not t:
        return ""
    return t.split(".")[0]


def _resolve_website(ticker: str, website: Optional[str] = None) -> str:
    w = str(website or "").strip()
    if w:
        return w
    try:
        import yfinance as yf  # local import — avoid hard dep at module import

        stock = yf.Ticker(str(ticker or "").strip())
        info = {}
        try:
            info = stock.info or {}
        except Exception:
            info = {}
        if not info.get("website"):
            gi = getattr(stock, "get_info", None)
            if callable(gi):
                try:
                    info2 = gi() or {}
                    if isinstance(info2, dict):
                        info = {**info, **info2}
                except Exception:
                    pass
        return str((info or {}).get("website") or (info or {}).get("irWebsite") or "").strip()
    except Exception:
        return ""


def _load_rgba(img_bytes: bytes) -> Optional["Image.Image"]:
    if Image is None or not img_bytes:
        return None
    try:
        im = Image.open(io.BytesIO(img_bytes))
    except Exception:
        return None
    best = None
    max_area = -1
    try:
        i = 0
        while True:
            im.seek(i)
            fr = im.convert("RGBA")
            area = fr.size[0] * fr.size[1]
            if area > max_area:
                best, max_area = fr, area
            i += 1
    except EOFError:
        pass
    except Exception:
        try:
            best = im.convert("RGBA")
        except Exception:
            return None
    return best


def extract_logo_colors_from_bytes(img_bytes: bytes, max_colors: int = 2) -> List[str]:
    """Return up to ``max_colors`` hex colors (#RRGGBB) from logo image bytes."""
    n = max(1, min(int(max_colors or 2), 2))
    im = _load_rgba(img_bytes)
    if im is None:
        return []
    try:
        im = im.resize((96, 96), Image.Resampling.LANCZOS)
    except Exception:
        im = im.resize((96, 96))

    scored: Counter = Counter()
    dark_mass = 0.0
    color_mass = 0.0
    for r, g, b, a in im.getdata():
        if a < 50:
            continue
        mx, mn = max(r, g, b), min(r, g, b)
        # near-white background
        if mx > 248 and mn > 235:
            continue
        s = _sat(r, g, b)
        v = mx / 255.0
        q = (int(r / 10) * 10, int(g / 10) * 10, int(b / 10) * 10)
        w = 1.0 + 3.0 * s + 0.4 * v
        if s < 0.12:
            w *= 0.18  # de-emphasize gray
        if mx < 28:
            # keep black/dark silhouettes (Apple, etc.) but lightly
            w *= 0.55
            dark_mass += w
        else:
            color_mass += w
            if s < 0.08:
                w *= 0.5
        scored[q] += w

    if not scored:
        return []

    mass_rank = [c for c, _ in scored.most_common(40)]

    # Prefer saturated brand accents among high-mass bins
    picks: List[Tuple[int, int, int]] = []
    for c in mass_rank:
        if _sat(*c) >= 0.16 or (dark_mass > color_mass * 1.2 and c[0] < 40):
            picks.append(c)
            break
    if not picks:
        picks.append(mass_rank[0])

    for c in mass_rank:
        if len(picks) >= n:
            break
        if any(_dist(c, p) < 52 for p in picks):
            continue
        # second color: require some chroma unless first was dark-only
        if _sat(*c) < 0.12 and _sat(*picks[0]) >= 0.16:
            continue
        picks.append(c)

    # Prefer text-safe colors on light UI; fall back to first pick if all rejected.
    safe = [p for p in picks if _ok_for_light_text(*p)]
    if not safe and picks:
        # Darken an overly light brand wash so the name stays visible
        r, g, b = picks[0]
        scale = 0.55
        safe = [(max(0, int(r * scale)), max(0, int(g * scale)), max(0, int(b * scale)))]
    out = ["#%02X%02X%02X" % (p[0], p[1], p[2]) for p in safe[:n]]
    return [h for h in out if _HEX_RE.match(h)]


def _fetch_logo_bytes(ticker: str, website: Optional[str] = None) -> Tuple[Optional[bytes], str]:
    sym = _ticker_symbol(ticker)
    domain = _domain_from_website(website)
    urls: List[str] = []
    if sym:
        urls.append(f"https://financialmodelingprep.com/image-stock/{sym}.png")
        urls.append(f"https://assets.parqet.com/logos/symbol/{sym}?format=png")
    if domain:
        urls.append(f"https://icons.duckduckgo.com/ip3/{domain}.ico")
        urls.append(f"https://www.google.com/s2/favicons?domain={domain}&sz=128")

    for u in urls:
        try:
            r = requests.get(u, timeout=8, headers=_UA, allow_redirects=True)
        except Exception:
            continue
        if r.status_code != 200 or not r.content or len(r.content) < 80:
            continue
        ct = (r.headers.get("content-type") or "").lower()
        if "svg" in ct:
            continue
        if ("image" not in ct) and (not u.lower().endswith((".png", ".ico", ".jpg", ".jpeg", ".webp"))):
            continue
        # skip tiny Google default globe placeholders
        if len(r.content) < 200 and "favicon" in u:
            continue
        return r.content, u
    return None, ""


def resolve_corp_logo_colors(
    ticker: str = "AAPL",
    website: Optional[str] = None,
    max_colors: int = 2,
) -> Dict[str, Any]:
    """Resolve up to two primary logo colors for ``ticker``.

    Returns a plain dict safe for reticulate:
      colors: list of "#RRGGBB" (length 0–2)
      source: logo URL used (or "")
      domain: website host (or "")
      ticker: normalized input ticker
    """
    tk = str(ticker or "").strip()
    empty = {"colors": [], "source": "", "domain": "", "ticker": tk}
    if not tk or Image is None:
        return empty

    web = _resolve_website(tk, website)
    domain = _domain_from_website(web)
    data, src = _fetch_logo_bytes(tk, web)
    if not data:
        return {**empty, "domain": domain}

    colors = extract_logo_colors_from_bytes(data, max_colors=max_colors)
    return {
        "colors": colors,
        "source": src or "",
        "domain": domain,
        "ticker": tk,
    }


__all__ = [
    "extract_logo_colors_from_bytes",
    "resolve_corp_logo_colors",
]
