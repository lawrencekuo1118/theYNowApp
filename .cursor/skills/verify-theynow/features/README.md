# theYNowApp verification map

This directory is the maintained source for verifying user-facing behavior of theYNowApp (`app_27.0/` Shiny UI). Read this index before driving the app, then use the matching feature file as the recipe.

## Baseline preconditions

- Launch via `bash .cursor/skills/verify-theynow/scripts/verify-launch.sh` so the instance owns an isolated port (default `13838`, not shared `3838`).
- Export `YNOW_VERIFY_RUN_DIR` from launch output (or keep the env the script printed).
- Run `bash .cursor/skills/verify-theynow/scripts/verify-doctor.sh` and require pass (process, port ownership, HTTP 200, “The YNow App” identity).
- Never drive an instance that was not started by this verification run (refuse shared Cloud `3838` unless exclusively owned and `YNOW_VERIFY_ALLOW_SHARED=1`).
- Offline `app_27.0/tests/*.R` may already be green; still drive the UI for map proof.
- Prefer Guest / signed-out Full surface unless a feature Gotcha requires auth.

## Driving conventions

- Start every recipe from a fresh page load of `$URL` unless preconditions say otherwise.
- Prefer stable ids / `aria-label` / `data-value` tab links over coordinates.
- Treat selector strings and helper flags as literal.
- Browser actions: Cursor `computerUse` subagent, or `node .cursor/skills/verify-theynow/scripts/verify-cdp-shot.mjs …`.
- Restore nothing destructive in Guest mode; do not sign out other sessions; do not deploy shinyapps from verification.
- Cleanup kills only the launched PID; proof artifacts stay under `/cursor/stores/self/media/verify-theynow/<RUN_ID>/` (or the skill `artifacts/` / `/opt/cursor/artifacts/verify-theynow/` fallbacks).

## Proof and skip reporting

- Capture the user action and the resulting state, not only the final screen.
- UI proof includes a screenshot with app identity visible (`The YNow App` / `data-ynow-build`) plus a DOM text dump of key selectors.
- Record the feature ID and entry point with every artifact (`PROOF.md`).
- Report an unreachable path with the attempted command and the unmet precondition (e.g. auth gate).
- Do not report a skipped entry point as verified through a different path.

## Feature entry contract

Each feature file starts with an H1 title and one paragraph describing the user-visible behavior. It then uses exactly four H2 sections in this order.

1. `Sub-features`
2. `How to get to it (user POV)`
3. `Driving it with verify-theynow`
4. `Gotchas`

Keep implementation details out of the map. Name only user paths, stable handles, required state, commands, and observable proof.

## Features

- [Home and Lite toggle](./home-lite.md) — brand Home, Lite/Full logo toggle, Home cards.
- [Ticker search to Company](./ticker-search.md) — `#sc` + Search → Company chrome / corp name.
- [About page](./about.md) — About · Privacy from Home and sidebar About.
- [Market and locale header](./market-locale.md) — US/TW market and EN/繁中 locale buttons.
- [Macro and Market Trends](./macro-market.md) — sidebar / Home card → Macro page title.
