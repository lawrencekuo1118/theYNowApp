# Home and Lite toggle

Home is the default landing tab: brand logo, title “The YNow App”, desk lead copy, navigation cards, and a logo control that toggles Lite vs Full mode for the session.

## Sub-features

- `home-land` loads Home with brand identity visible.
- `home-card-nav` jumps to a mapped tab via a Home card (`data-tab`).
- `home-lite-toggle` switches Lite / Full via the logo control and shows the LITE badge path.
- `home-about-link` opens About from the Home legal row.

## How to get to it (user POV)

- Open the app URL (default tab is Home).
- Choose sidebar **Home** (`#ynow_menu_home` / `a[data-value="home"]`).
- On Home, click the large logo (`#ynow_home_lite_toggle`) to toggle Lite.
- Click a Home card (Market, Blue Chip, Company, YNOW, Fundamental Value / Smart Analysis, Action).
- Click **About · Privacy** (`#ynow_home_legal_about`).

## Driving it with verify-theynow

Preconditions:

- Doctor passed for this run’s `$URL`.
- Guest session; start at `$URL/` after a fresh load.

- **Confirm landing.** Open `$URL`. Observe `#ynow_home_title` text `The YNow App` and `.ynow-app-title-base` containing the display version. Capture `home-before.png` + DOM dump.
- **Toggle Lite.** Click `#ynow_home_lite_toggle` (`aria-label="Toggle Lite mode"`). `document.body` gains class `ynow-lite` and `#ynow_home_lite_badge` is visible. Capture `home-lite-on.png`.
- **Toggle Full.** Click `#ynow_home_lite_toggle` again. `body.ynow-lite` is removed. Capture `home-lite-off.png`.
- **Card navigation.** Click the Home card with `data-tab="macro_market"`. Macro page shows `#ynow_macro_page_title` (may need a short wait for lazy content).
- **Proof.** Artifacts show brand title before toggle, `ynow-lite` after first toggle, and Macro title after the card click. Record entry point `home-land` / `home-lite-toggle` in `PROOF.md`.

Example CDP:

```bash
node .cursor/skills/verify-theynow/scripts/verify-cdp-shot.mjs \
  --url "$URL" --out "$EVIDENCE_DIR/home-land" \
  --must-contain 'The YNow App'
```

## Gotchas

- Sidebar is collapsed by default; Home cards avoid needing the hamburger for card nav.
- Lite hides some Full-only Home cards (`ynow-full-only`); Value card uses `data-tab-lite="smart_analysis"`.
- Logo toggle is on Home only (`#ynow_home_lite_toggle`), not the small header mark (`#ynow_header_logo_home` refreshes the current tab).
- Do not treat offline unit tests as proof of Lite CSS visibility.
