#!/usr/bin/env python3
"""Build tall AdminLTE-style HTML fixtures for gate-demo scroll capture."""
from __future__ import annotations

from pathlib import Path

OUT = Path(__file__).resolve().parent

CSS = """
:root { --ink:#1a1a1a; --muted:#555; --line:#d0d0d0; --accent:#249c60; --gold:#c9a227; }
* { box-sizing: border-box; }
html, body { height:100%; overflow:hidden; }
body { margin:0; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
  background:#ececec; color:var(--ink); }
.top { background:#1a1a1a; color:#f5e6a8; padding:10px 16px; font-weight:700; letter-spacing:.02em; }
.wrap { display:flex; height:calc(100vh - 40px); overflow:hidden; }
.side { width:220px; background:#222d32; color:#b8c7ce; padding:12px 0; flex:0 0 auto; overflow:auto; }
.side a { display:block; padding:8px 14px; color:#b8c7ce; text-decoration:none; font-size:13px; }
.side a.active { background:#1e282c; color:#fff; border-left:3px solid var(--accent); }
.main { flex:1; padding:16px 18px 80px; max-width:900px; overflow-y:auto; height:100%; }
.h2 { margin:0 0 6px; font-size:22px; }
.lead { margin:0 0 14px; color:var(--muted); font-size:13px; line-height:1.45; }
.row { display:flex; gap:12px; flex-wrap:wrap; margin:0 0 14px; }
.kpi { flex:1 1 140px; background:#fff; border:1px solid var(--line); border-radius:4px; padding:12px; min-height:88px; }
.kpi .l { font-size:11px; color:var(--muted); text-transform:uppercase; letter-spacing:.04em; }
.kpi .v { font-size:22px; font-weight:700; margin-top:6px; }
.kpi .c { font-size:12px; color:var(--accent); margin-top:4px; }
.box { background:#fff; border:1px solid var(--line); border-top:3px solid var(--ink); border-radius:3px; margin:0 0 14px; }
.box-h { padding:10px 12px; border-bottom:1px solid var(--line); font-weight:700; font-size:14px; }
.box-b { padding:12px; font-size:13px; line-height:1.5; color:#333; }
table { width:100%; border-collapse:collapse; font-size:12px; }
th,td { border-bottom:1px solid #eee; padding:7px 6px; text-align:left; }
th { color:#666; font-weight:600; }
.chart { height:180px; background:linear-gradient(180deg,#fafafa,#f0f0f0);
  border:1px dashed #ccc; border-radius:4px; position:relative; overflow:hidden; }
.chart svg { width:100%; height:100%; display:block; }
.tag { display:inline-block; padding:2px 8px; border:1px solid #000; background:#1a1a1a; color:#fff;
  font-size:11px; font-weight:700; letter-spacing:.06em; margin-right:6px; }
.note { font-size:12px; color:var(--muted); margin-top:8px; }
"""


def chart_svg(seed: int = 1) -> str:
    pts = []
    y = 120
    for i in range(0, 21):
        y += ((i * seed * 17) % 23) - 11
        y = max(40, min(160, y))
        pts.append(f"{20 + i * 42},{y}")
    poly = " ".join(pts)
    return f"""<svg viewBox="0 0 900 180" xmlns="http://www.w3.org/2000/svg">
      <polyline fill="none" stroke="#249c60" stroke-width="3" points="{poly}"/>
      <line x1="20" y1="160" x2="880" y2="160" stroke="#ccc"/>
    </svg>"""


def page(feat: str, title: str, lead: str, sections: list[str]) -> str:
    side_items = [
        ("lab", "Quant Backtest Lab"),
        ("checklist", "Decision Checklist"),
        ("alerts", "Value Alerts"),
        ("account", "Account"),
        ("pdf", "Company / PDF"),
        ("snap_defaults", "Snapshot defaults"),
        ("inner_lock", "Locked tab"),
        ("atx", "Asset Transmission"),
    ]
    side = "\n".join(
        f'<a class="{"active" if k==feat else ""}" href="#">{lab}</a>' for k, lab in side_items
    )
    body = "\n".join(sections)
    return f"""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>{title}</title><style>{CSS}</style></head>
<body>
<div class="top">The YNow App · {title}</div>
<div class="wrap">
  <nav class="side">{side}</nav>
  <main class="main" id="scroll-root">
    <div class="h2">{title}</div>
    <p class="lead"><span class="tag">DEMO</span>{lead}</p>
    {body}
  </main>
</div>
</body></html>"""


def kpi_row(items):
    cells = "".join(
        f'<div class="kpi"><div class="l">{l}</div><div class="v">{v}</div><div class="c">{c}</div></div>'
        for l, v, c in items
    )
    return f'<div class="row">{cells}</div>'


def box(title, inner):
    return f'<div class="box"><div class="box-h">{title}</div><div class="box-b">{inner}</div></div>'


PAGES = {}

PAGES["lab"] = page(
    "lab",
    "Quantitative Backtest Lab",
    "HFV + Quant worksheets, Fair Value replay, and Lab reports (APP member surface).",
    [
        kpi_row([
            ("HFV status", "Ready", "veto tool only"),
            ("Backtest runs", "12", "+2 this week"),
            ("FYN spent", "35", "Lab runs"),
            ("Universe", "US", "GICS peers"),
        ]),
        box("Fair Value replay", f'<div class="chart">{chart_svg(2)}</div><p class="note">Replay path vs close — research display only.</p>'),
        box(
            "Quant worksheet",
            """<table><tr><th>Metric</th><th>Base</th><th>Bear</th><th>Bull</th></tr>
            <tr><td>MOS</td><td>18%</td><td>32%</td><td>8%</td></tr>
            <tr><td>WACC</td><td>9.2%</td><td>10.1%</td><td>8.4%</td></tr>
            <tr><td>Terminal g</td><td>2.5%</td><td>1.5%</td><td>3.0%</td></tr>
            <tr><td>FCFF CAGR</td><td>7%</td><td>3%</td><td>11%</td></tr>
            <tr><td>Ke (CAPM)</td><td>10.4%</td><td>11.2%</td><td>9.6%</td></tr>
            </table>""",
        ),
        box("Lab notes / methodology", "<p>Two-Stage DCF with CapEx spike smoothing. HFV remains a veto — never a buy signal. Rolling windows use native currency series (no historical FX).</p>" * 4),
        box("Run log", "<p>2026-10-18 · AAPL · Quant pass · 5 FYN</p><p>2026-10-17 · MSFT · HFV veto · 0 FYN</p><p>2026-10-15 · NVDA · Quant pass · 5 FYN</p>" + "<p>… additional Lab history rows for scroll depth …</p>" * 12),
    ],
)

PAGES["checklist"] = page(
    "checklist",
    "Decision Checklist",
    "Mandatory Decision SOP gates before a YNOW verdict.",
    [
        kpi_row([("Gates passed", "4 / 7", "in progress"), ("F-Score", "7", "quality OK"), ("MOS Bear", "24%", "pass"), ("HFV", "Clear", "no veto")]),
        box("SOP gates", """<table><tr><th>Gate</th><th>Status</th><th>Note</th></tr>
        <tr><td>Bear MOS</td><td>Pass</td><td>≥20%</td></tr>
        <tr><td>Base MOS</td><td>Pass</td><td>≥10%</td></tr>
        <tr><td>F-Score</td><td>Pass</td><td>≥6</td></tr>
        <tr><td>Earnings quality</td><td>Review</td><td>Accruals elevated</td></tr>
        <tr><td>HFV veto</td><td>Pass</td><td>No veto</td></tr>
        <tr><td>Liquidity</td><td>Pass</td><td>ADV OK</td></tr>
        <tr><td>Thesis write-up</td><td>Open</td><td>Required</td></tr></table>"""),
        box("Checklist narrative", "<p>Decision Checklist is a desk SOP — not a marketing score. Complete every gate before unlocking the YNOW verdict panel.</p>" * 8),
        box("History", "<p>Prior SOP runs and gate diffs…</p>" * 15),
    ],
)

PAGES["alerts"] = page(
    "alerts",
    "Value Alerts",
    "Save price / value rules and email notify to your account mailbox.",
    [
        kpi_row([("Active rules", "6", "2 firing"), ("Slots used", "6 FYN", "alert_slot"), ("Mailbox", "on", "account email"), ("History", "48", "30d")]),
        box("Rules", """<table><tr><th>Ticker</th><th>Metric</th><th>Kind</th><th>Threshold</th></tr>
        <tr><td>AAPL</td><td>last_price</td><td>≤</td><td>180</td></tr>
        <tr><td>2330</td><td>MOS</td><td>≥</td><td>25%</td></tr>
        <tr><td>MSFT</td><td>FV gap</td><td>≥</td><td>15%</td></tr>
        <tr><td>TSM</td><td>last_price</td><td>≤</td><td>140</td></tr></table>"""),
        box("Alert chart", f'<div class="chart">{chart_svg(3)}</div>'),
        box("Notify log", "<p>Email notify ledger and delivery status…</p>" * 16),
    ],
)

PAGES["account"] = page(
    "account",
    "Account workspace",
    "Overview, themes, FYN / PYN balances, and the Points ledger.",
    [
        kpi_row([("FYN", "128", "spendable"), ("PYN", "40", "premium"), ("Check-in", "Done", "+30 FYN"), ("Theme", "Classic", "page theme")]),
        box("Overview", "<p>Signed-in APP member workspace. Daily check-in and welcome gift appear on the ledger.</p>" * 5),
        box("Points ledger", """<table><tr><th>When</th><th>SKU</th><th>Δ FYN</th><th>Balance</th></tr>
        <tr><td>Today</td><td>check-in</td><td>+30</td><td>128</td></tr>
        <tr><td>Yesterday</td><td>bt_run</td><td>−5</td><td>98</td></tr>
        <tr><td>Oct 18</td><td>alert_slot</td><td>−1</td><td>103</td></tr></table>""" + "<p>…</p>" * 14),
    ],
)

PAGES["pdf"] = page(
    "pdf",
    "Valuation PDF download",
    "Broker-style report for the active ticker — three free downloads / Asia/Taipei day.",
    [
        kpi_row([("Ticker", "AAPL", "Company"), ("Quota", "2 left", "today"), ("Extra", "10 FYN", "per PDF"), ("Pages", "18", "report")]),
        box("Report preview", f'<div class="chart">{chart_svg(5)}</div><p class="note">Cover sheet + DCF / MOS / Decision Checklist excerpts.</p>'),
        box("Sections", "<p>1. Snapshot</p><p>2. FCFF bridge</p><p>3. WACC / Ke</p><p>4. Terminal Value</p><p>5. MOS Bear/Base</p>" + "<p>Appendix …</p>" * 18),
    ],
)

PAGES["snap_defaults"] = page(
    "snap_defaults",
    "System defaults (APP_DEFAULTS)",
    "Sysadmin unlock to view / edit live-line APP_DEFAULTS and restore CSV.",
    [
        kpi_row([("Mode", "Sysadmin", "unlocked"), ("Keys", "96", "APP_DEFAULTS"), ("CSV", "Ready", "download"), ("Lock", "On exit", "re-locks")]),
        box("Defaults table", """<table><tr><th>Key</th><th>Value</th><th>Note</th></tr>
        <tr><td>perpetual_g</td><td>0.025</td><td>terminal g</td></tr>
        <tr><td>wacc_floor</td><td>0.06</td><td>engineering floor</td></tr>
        <tr><td>capex_spike_mult</td><td>1.35</td><td>smoothing</td></tr></table>""" + "<p>… more defaults …</p>" * 20),
    ],
)

PAGES["inner_lock"] = page(
    "inner_lock",
    "Locked tab surface",
    "Lock icon replaces the tab title while restricted; other tabs stay available.",
    [
        kpi_row([("State", "Locked", "sign-in / admin"), ("Tab", "Hist. FV", "example"), ("Sibling tabs", "Open", "usable"), ("Unlock", "Admin", "clear lock")]),
        box("What you see", "<p>While locked, the panel shows a Sign-in / admin unlock gate. Sibling tabs on the same page remain usable.</p>" * 6),
        box("After unlock", f'<div class="chart">{chart_svg(4)}</div>' + "<p>Unlocked content loads in place…</p>" * 12),
    ],
)

PAGES["atx"] = page(
    "atx",
    "Asset Transmission Root",
    "Macro drivers → rate / dollar / liquidity hubs → asset classes (surveillance map).",
    [
        kpi_row([("Regime", "Neutral", "not a trade signal"), ("Hubs", "3", "rates · FX · liq"), ("Assets", "5", "mapped"), ("Mode", "Full", "unlock")]),
        box("Transmission map", f'<div class="chart">{chart_svg(6)}</div><p class="note">Root map is surveillance only — never a buy/sell signal.</p>'),
        box("Macro drivers", "<p>Inflation proxy, growth pulse, policy path…</p>" * 4),
        box("Asset classes", "<p>Bonds · Equities · Metals · FX · Crypto — hub linkages and stress tags.</p>" * 10),
        box("Method notes", "<p>Regime card is descriptive. Do not treat ATX as a trading system.</p>" * 8),
    ],
)


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    for feat, html in PAGES.items():
        path = OUT / f"{feat}.html"
        path.write_text(html, encoding="utf-8")
        print("wrote", path.name)


if __name__ == "__main__":
    main()
