# Macro and Market Trends

Macro & Market Trends shows market-mode KPI / index / theme boards (YNOW or TYNOW, Rf, industry vs benchmark) for the active US or TW subscription.

## Sub-features

- `macro-open-sidebar` opens the tab from the sidebar.
- `macro-open-home-card` opens the tab from the Home Market card.
- `macro-title` shows `#ynow_macro_page_title` after lazy mount.
- `macro-market-sensitive` reflects US vs TW after header market switch (optional follow-on).

## How to get to it (user POV)

- Sidebar: **Macro & Market Trends** (`#ynow_menu_macro` / `a[data-value="macro_market"]`).
- Home: **Market** card (`button.ynow-home-card[data-tab="macro_market"]`).

## Driving it with verify-theynow

Preconditions:

- Doctor passed.
- Prefer Home card entry so a collapsed sidebar does not block the path.
- Network may be required for live index widgets; title/shell must still appear even if some KPI cells stay loading — assert the page title first. If the entire tab errors, fail the journey (do not weaken to “title skipped”).

- **Home card entry.** From Home, click the Market card (`[data-tab="macro_market"]`). Wait for `#ynow_macro_page_title`. Capture screenshot + DOM.
- **Sidebar entry.** From Home, open sidebar, click `a[data-value="macro_market"]` (or `#ynow_menu_macro`’s parent link). Same title visible.
- **Optional market follow-on.** With Macro open, click `#ynow_mkt_btn_tw`, wait for refresh, confirm Macro shell still present (TYNOW path). Revert to US if continuing other features.
- **Proof.** `macroTitle` in DOM dump is non-empty; screenshot shows Macro page chrome. Record entry point used.

```bash
node .cursor/skills/verify-theynow/scripts/verify-cdp-shot.mjs \
  --url "$URL" \
  --out "$EVIDENCE_DIR/macro-home-card" \
  --click 'button.ynow-home-card[data-tab="macro_market"]' \
  --wait '#ynow_macro_page_title' \
  --must-contain 'Macro'
```

(Adjust `--must-contain` to the live locale string if EN copy differs; prefer asserting `#ynow_macro_page_title` text from `dom.json`.)

## Gotchas

- Macro UI is market-sensitive; stale US widgets after a TW switch are a real bug — wait for settle.
- Collapsed sidebar: expand before using the menu path, or use the Home card.
- Heavy charts (plotly) can lag on Cloud VMs — wait for the title element, not every sparkline.
- Guest users can open Macro; do not require sign-in for this feature’s baseline proof.
