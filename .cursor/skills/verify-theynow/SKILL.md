---
name: verify-theynow
description: "Drive theYNowApp (Shiny R, app_27.0/) in a real browser the way a user would — Home, ticker Search, About, US/TW + locale, Macro. Use when proving UI behavior, debugging regressions, or after live-line UI changes. Prefer this over offline app_27.0/tests/*.R alone."
---

# Verify theYNowApp

Project-local verification skill for **theYNowApp** (live line `app_27.0/`). An agent reading this cold must launch an isolated Shiny instance, doctor it, drive one mapped feature through the real web UI, capture evidence, then clean up **without** deleting proof artifacts.

Offline unit checks under `app_27.0/tests/*.R` are useful gates; they are **not** a substitute for UI drive.

Production reference (do not treat as the verification target unless explicitly asked): https://hopesmasher1118.shinyapps.io/TheYNowApp/

## Surface

- **Primary:** Shiny web UI (AdminLTE / shinydashboard) served from `app_27.0/`.
- **Also present (not this skill’s default drive):** R unit tests, PDF report render, shinyapps deploy scripts. Note them; do not claim UI proof from them.

## Launch

Prefer an **isolated verify port** so Cloud Agents that already bind `3838` are not double-driven.

```bash
# From repo root. Creates a run dir, starts Shiny, waits until HTTP 200.
RUN_ID="${RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)-$$}"
export YNOW_VERIFY_PORT="${YNOW_VERIFY_PORT:-13838}"
export YNOW_VERIFY_RUN_DIR="${YNOW_VERIFY_RUN_DIR:-/tmp/ynow-verify-$RUN_ID}"
bash .cursor/skills/verify-theynow/scripts/verify-launch.sh
```

What launch does:

1. Refuses if `$YNOW_VERIFY_PORT` is already listening (no steal / no share).
2. Starts `Rscript -e 'shiny::runApp(host="127.0.0.1", port=<port>, launch.browser=FALSE)'` from `app_27.0/` in the background.
3. Writes `$YNOW_VERIFY_RUN_DIR/meta.env` (`PID`, `PORT`, `URL`, `RUN_ID`, `STARTED_AT`) and a log at `$YNOW_VERIFY_RUN_DIR/shiny.log`.
4. Polls `http://127.0.0.1:$YNOW_VERIFY_PORT/` until HTTP 200 **and** the HTML contains `The YNow App` (usable UI, not just a socket).

Cloud default start (`bash scripts/cloud-agent-start.sh` → `0.0.0.0:3838`) is the shared Cloud Agent boot path. **Do not drive that shared instance** from this skill. If you only have 3838 and it is already owned by another session, stop and report isolation failure — do not attach a second browser drive.

Ready signal: `verify-doctor.sh` exits 0 (see Doctor).

Teardown: `bash .cursor/skills/verify-theynow/scripts/verify-cleanup.sh` (kills only the PID in `meta.env`).

Bootstrap (once per VM if R/packages missing): `bash scripts/cloud-agent-install.sh`.

## Doctor

Read-only. Run before every drive and whenever anything looks off:

```bash
# Requires YNOW_VERIFY_RUN_DIR pointing at the launch meta dir.
bash .cursor/skills/verify-theynow/scripts/verify-doctor.sh
```

Doctor answers:

| Check | Pass criterion |
|-------|----------------|
| meta | `$YNOW_VERIFY_RUN_DIR/meta.env` exists and exports `PID`, `PORT`, `URL` |
| process | `kill -0 $PID` succeeds |
| port ownership | listener on `$PORT` is that same PID (via `ss`/`lsof`) |
| HTTP | `GET $URL/` → 200 |
| identity | body contains `The YNow App` and `data-ynow-build` matching `app_27.0/www/ynow_build.json` `version` when present |
| isolation | `$PORT` is **not** the shared Cloud boot port `3838` unless `YNOW_VERIFY_ALLOW_SHARED=1` was set at launch (default: refuse 3838) |

Exit non-zero on any failure. Do not drive a failing instance.

## Drive

Harness: **browser / CDP** (Chrome). Prefer the Cursor `computerUse` subagent or a Chrome DevTools session against `$URL`. There is no Playwright suite in-repo for the full app; drive the live DOM.

Stable handles (prefer these over coordinates):

| Handle | Role |
|--------|------|
| `#ynow_home_title` | Home brand H2 “The YNow App” |
| `#ynow_home_lite_toggle` | Lite / Full logo toggle (`aria-label="Toggle Lite mode"`) |
| `#ynow_menu_home` | Sidebar Home label |
| `a[data-value="home"]` / `a[data-value="about"]` / `a[data-value="macro_market"]` / `a[data-value="dashboard"]` | shinydashboard sidebar tab links |
| `#sc` | Ticker / Stock Code text input |
| `#search` / `button#search` | Search action button |
| `#txt_corpname` | Company name after a successful search |
| `#search_results` | Verbatim search / industry text |
| `#ynow_mkt_btn_us` / `#ynow_mkt_btn_tw` | Market US / TW header buttons (`aria-label`) |
| `#ynow_lang_btn_en` / `#ynow_lang_btn_zh` | Locale EN / 繁中 |
| `#ynow_home_legal_about` | Home → About · Privacy |
| `.ynow-about-title` | About page heading (lazy tab; wait after navigation) |
| `#ynow_macro_page_title` | Macro & Market Trends page title |
| `html[data-ynow-build]` / `.ynow-app-title-base` | Build / display version identity |

Shiny notes:

- Sidebar starts **collapsed**; open it or use Home cards / `window.ynowGotoTab('<tabName>')` only when proving a path that already exposes that JS (user-reachable Home cards do). Prefer clicking visible UI.
- About and several tabs are **lazy** (`ynow_lazy_host_*`). After clicking the tab, wait until the host’s content appears (e.g. `.ynow-about-title`).
- Guest locks: Decision Checklist / Quant Backtest / Value Alerts / some Full features require sign-in. Do not weaken journeys — report the auth gate in that feature’s Gotchas.
- i18n: after `#ynow_lang_btn_zh`, assert Taiwan Traditional Chinese copy (not Simplified). Keep English finance terms (WACC, FCFF, MOS, …).

Feature recipes live in [`features/`](features/README.md). Drive **one** mapped feature per proof unless maintaining the map.

Optional CDP helper (headless Chrome screenshot + DOM text probe):

```bash
# After doctor passes; example: About journey.
node .cursor/skills/verify-theynow/scripts/verify-cdp-shot.mjs \
  --url "$URL" \
  --out "$EVIDENCE_DIR/about" \
  --click 'a[data-value="about"]' \
  --wait '.ynow-about-title' \
  --must-contain 'About The YNow App'
```

## Evidence

Proof standards:

1. Exercise the **real user path** (sidebar / Home card / Search button) — not internal `updateTabItems` from a test-only R helper.
2. Capture **action + resulting state** (before/after screenshots or ARIA/DOM text dumps), not only the final frame.
3. Verify side effects when relevant (ticker chrome shows corp name; locale button gains `.active`; Lite sets `body.ynow-lite`).
4. Mocks only at production boundaries already isolated (do not stub the Shiny UI).

Artifact locations (survive cleanup):

| Where | When |
|-------|------|
| `/cursor/stores/self/media/verify-theynow/<RUN_ID>/` | Agent Store available (preferred for handoff) |
| `.cursor/skills/verify-theynow/artifacts/<RUN_ID>/` | Always writable in the clone; **gitignored content OK** — keep locally for the run |
| `/opt/cursor/artifacts/verify-theynow/<RUN_ID>/` | Cloud walkthrough upload when present |

Minimum set per proof:

- `doctor.txt` — doctor stdout
- `before.png` / `after.png` (or named steps)
- `dom-after.txt` — textContent / selected selectors
- `PROOF.md` — feature ID, entry point, URL, port, PID, pass/fail

Never put secrets (`.Renviron`, tokens) into evidence.

## Cleanup

```bash
bash .cursor/skills/verify-theynow/scripts/verify-cleanup.sh
```

- Kills **only** the PID recorded in `$YNOW_VERIFY_RUN_DIR/meta.env`.
- Removes the run scratch dir **except** when `YNOW_VERIFY_KEEP_RUN_DIR=1`.
- **Never** deletes evidence under `/cursor/stores/self/media/verify-theynow/`, `/opt/cursor/artifacts/verify-theynow/`, or `artifacts/<RUN_ID>/` proof files.
- Never `pkill -f shiny` / never kill by process name.

After cleanup, confirm evidence paths still exist before reporting success.

## Helpers

All under `.cursor/skills/verify-theynow/scripts/` (executable):

| Script | Purpose |
|--------|---------|
| `verify-launch.sh` | Isolated Shiny start + ready wait |
| `verify-doctor.sh` | Read-only health / ownership / identity |
| `verify-cleanup.sh` | Tear down launched PID only |
| `verify-cdp-shot.mjs` | Headless Chrome: open URL, optional click/wait, screenshot + text dump |

Invocation examples are in Launch / Doctor / Drive above. Source `meta.env` after launch:

```bash
# shellcheck disable=SC1090
source "$YNOW_VERIFY_RUN_DIR/meta.env"
echo "$URL"
```

## Feature map

See [`features/README.md`](features/README.md). Seeded: Home / Lite, ticker Search → Company, About, market+locale header, Macro & Market Trends.

Maintenance: `/maintain-verification-skill` when UI handles drift.
