# Cloud tip-ship (GitHub Actions)

Tip-ship the live line (`app_28.0/`) to shinyapps.io **without** the Mac coding clone.

Workflow: [`.github/workflows/tip-ship.yml`](../.github/workflows/tip-ship.yml)  
Deploy entrypoint: `Rscript scripts/deploy_app_21.R`  
Baseline recorder: `Rscript scripts/record_deploy_baseline.R`  
Land helper: `bash scripts/tip_ship_land_commits.sh`

## How auto-deploy works (no UI click)

1. Merge (or push) to **`master`** changes under `app_28.0/**`, `scripts/**`, or `.github/workflows/tip-ship.yml`.
2. **Live-line CI** runs offline checks on that push.
3. On **success**, Tip-ship starts via `workflow_run` (no `workflow_dispatch`, no PAT).
4. Gate skips: `github-actions[bot]` pushes (bump/baseline land → no ship loop), chore bump/baseline messages, and non-ship paths (e.g. README/CHANGELOG-only).
5. Ships only when branch tip still equals the green CI SHA (if master moved, a newer CI owns the tip).

Cloud agents still get **403** on `workflow_dispatch` — that is fine: **merge to master is enough** for ship. Manual **Actions → Tip-ship (shinyapps) → Run workflow** remains as a fallback / retry (`skip_bump`).

## Lawrence checklist — repo Actions secrets

Add under **Settings → Secrets and variables → Actions** (names only; never commit values):

| Secret | Required | Source |
|---|---|---|
| `SHINYAPPS_ACCOUNT` | **yes** | shinyapps.io account name |
| `SHINYAPPS_TOKEN` | **yes** | https://www.shinyapps.io/admin/#/tokens |
| `SHINYAPPS_SECRET` | **yes** | same tokens page |

**Land path (no PAT / no ruleset bypass):** deploy → bump + baseline commits on the runner → push `tip-ship/baseline-*` with job `GITHUB_TOKEN` → open PR into the ship branch → dispatch Live-line CI (token events do not fire `pull_request`) → publish `offline-checks` status when needed → enable auto-merge → wait until **MERGED** → Release. Superseded open `tip-ship/baseline-*` PRs are auto-closed.

Do **not** add `TIP_SHIP_GH_PAT`. Do **not** require `github-actions[bot]` ruleset bypass for tip-ship land.

Optional repo **variables** (not secrets):

| Variable | Default if unset |
|---|---|
| `SHINYAPPS_APP_NAME` | `TheYNowApp` |
| `SHINYAPPS_APP_ID` | `10907657` |

Allow-listed `YNOW_*` secrets — **required when live restore alone lacks OAuth + account DB** (do **not** ship a thinner `.Renviron`; do not set `YNOW_ALLOW_MISSING_*`):

- `YNOW_DB_HOST`, `YNOW_DB_NAME`, `YNOW_DB_USER`, `YNOW_DB_PASSWORD`, `YNOW_DB_PORT`, `YNOW_DB_SSLMODE`, `YNOW_DATABASE_URL`
- `YNOW_SUPABASE_URL`, `YNOW_SUPABASE_ANON_KEY`, `YNOW_SUPABASE_SERVICE_ROLE`
- `YNOW_GOOGLE_CLIENT_ID`, `YNOW_GOOGLE_CLIENT_SECRET`

`deploy_app_21.R` restores live `.Renviron` by default (workflow pre-configures `rsconnect::setAccountInfo` from `SHINYAPPS_*`), then merges any of the above from process env via `inject_renviron_from_env.R`.

## What the workflow does

1. Guards that `SHINYAPPS_ACCOUNT` / `TOKEN` / `SECRET` exist (fails with a checklist in the job summary if not).
2. Checks out a **branch** only (default `master`); refuses detached SHA/tag refs.
3. Installs system libs (`libpq-dev`, `libsodium-dev`, xml/font libs, …) + the **full** app package set via `scripts/install_deploy_app_packages.R` (boot `p_load` + Neon/async + snapshot extras). Thin installs omit packages from the rsconnect manifest and cause shinyapps **exit status 1**. Configures rsconnect from `SHINYAPPS_*`.
4. Runs `deploy_app_21.R` with `YNOW_SKIP_GITHUB_RELEASE=1` (+0.01 bump → upload → `display_version` commit). Preflight refuses ship if required packages are missing.
5. Runs `record_deploy_baseline.R` (full `DEPLOY_BASELINE.txt` with live `bundleId` / commit).
6. **Lands** bump + baseline via `scripts/tip_ship_land_commits.sh` (PR + CI + auto-merge). Fails the job if the land PR never merges.
7. Runs `record_github_release.sh` targeting the **merged** SHA (fallback: ship tip).
8. Soft-skips Mac OneDrive sync via `post_release_local_sync.sh` → handoff (`PENDING_MAC_SYNC.env` + **Post-release Mac local sync**).

## If land still fails

Check the tip-ship job log for the land PR URL, Live-line CI run on `tip-ship/baseline-*`, and auto-merge state. Fix failing `offline-checks`, then re-run Tip-ship (optionally `skip_bump` if the upload already succeeded). Do **not** add `TIP_SHIP_GH_PAT` or a ruleset bypass for this path.

## Residual (Mac-owned)

- Coding clone + OneDrive physical backup sync remains **Mac-side** (launchd / self-hosted runner labels `self-hosted`, `macOS`, `theynow` / Cursor `agent worker start`).
- Prefer cloud tip-ship when the three `SHINYAPPS_*` secrets are configured; Mac remains preferred for local iteration and mandatory for post-release OneDrive catch-up.

## Out of scope / do not

- Do not paste secret values into PRs, chat, or YAML.
- Do not tip-ship paused PR #801 from this workflow setup work.
- Do not put shinyapps deploy steps into `.github/workflows/ci.yml` (keep Live-line CI offline-only; auto ship stays in `tip-ship.yml` via `workflow_run`).
