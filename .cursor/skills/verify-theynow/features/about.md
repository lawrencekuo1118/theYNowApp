# About page

About presents project / Lite introduction and legal notices (privacy, IP, investment risk). It is a lazy-loaded tab reachable from Home and the sidebar.

## Sub-features

- `about-home-link` opens About from Home **About · Privacy**.
- `about-sidebar` opens About from the sidebar menu item.
- `about-identity` shows the About heading and bilingual notice content.

## How to get to it (user POV)

- On Home, choose **About · Privacy** (`#ynow_home_legal_about`).
- Open the sidebar and choose **About** (`a[data-value="about"]`).

## Driving it with verify-theynow

Preconditions:

- Doctor passed at `$URL`.
- Fresh load on Home (default).

- **Home entry.** Click `#ynow_home_legal_about`. Wait for `.ynow-about-title`. Text includes `About The YNow App` / `關於 The YNow App` (Full) or the Lite variant when `body.ynow-lite` — locale follows the header EN/繁中 control.
- **Sidebar entry (second path).** Return to Home (`a[data-value="home"]`), open sidebar if needed, click `a[data-value="about"]`. Same About title appears.
- **Proof.** Screenshot with About title visible; `dom.txt` / `dom.json` `aboutTitle` matches. Record which entry point was driven. Capturing only one entry is a partial map proof — note the other as not yet driven this run.

```bash
node .cursor/skills/verify-theynow/scripts/verify-cdp-shot.mjs \
  --url "$URL" \
  --out "$EVIDENCE_DIR/about-home" \
  --click '#ynow_home_legal_about' \
  --wait '.ynow-about-title' \
  --must-contain 'The YNow App' \
  --sleep-ms 3000
```

## Gotchas

- About body is lazy (`ynow_lazy_host_about`). Clicking the tab before content mounts looks empty — wait for `.ynow-about-title`.
- Lite vs Full About copy differs; assert the heading that matches `body.ynow-lite`.
- Cookie consent may overlay the bottom of the page; dismiss with Accept / Reject if it blocks a click target (About home link is usually still reachable).
- Wait for `window.Shiny` before clicking Home legal links; a too-early click is a silent no-op.
- Do not scrape production shinyapps as a substitute for the local verify instance.
- Investment-risk copy is educational disclaimer, not a trade signal — still must be visible for legal path proof.
