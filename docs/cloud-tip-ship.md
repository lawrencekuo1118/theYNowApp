# Cloud tip-ship (GitHub Actions)

Tip-ship the live line (`app_28.0/`) to shinyapps.io **without** the Mac coding clone, via Actions → **Tip-ship (shinyapps)** (`workflow_dispatch`).

Workflow: [`.github/workflows/tip-ship.yml`](../.github/workflows/tip-ship.yml)  
Deploy entrypoint: `Rscript scripts/deploy_app_21.R`  
Baseline recorder: `Rscript scripts/record_deploy_baseline.R`

## Lawrence checklist — repo Actions secrets

Add under **Settings → Secrets and variables → Actions** (names only; never commit values):

| Secret | Required | Source |
|---|---|---|
| `SHINYAPPS_ACCOUNT` | **yes** | shinyapps.io account name |
| `SHINYAPPS_TOKEN` | **yes** | https://www.shinyapps.io/admin/#/tokens |
| `SHINYAPPS_SECRET` | **yes** | same tokens page |

Optional repo **variables** (not secrets):

| Variable | Default if unset |
|---|---|
| `SHINYAPPS_APP_NAME` | `TheYNowApp` |
| `SHINYAPPS_APP_ID` | `10907657` |

Optional allow-listed `YNOW_*` secrets (only if live `.Renviron` restore is incomplete):

- `YNOW_DB_HOST`, `YNOW_DB_NAME`, `YNOW_DB_USER`, `YNOW_DB_PASSWORD`, `YNOW_DB_PORT`, `YNOW_DB_SSLMODE`, `YNOW_DATABASE_URL`
- `YNOW_SUPABASE_URL`, `YNOW_SUPABASE_ANON_KEY`, `YNOW_SUPABASE_SERVICE_ROLE`
- `YNOW_GOOGLE_CLIENT_ID`, `YNOW_GOOGLE_CLIENT_SECRET`

`deploy_app_21.R` restores live `.Renviron` by default, then merges any of the above from process env via `inject_renviron_from_env.R`.

## What the workflow does

1. Guards that `SHINYAPPS_ACCOUNT` / `TOKEN` / `SECRET` exist (fails with a checklist in the job summary if not).
2. Sets up R + `rsconnect` and deploy preflight packages.
3. Runs `deploy_app_21.R` (+0.01 bump commit → upload → `display_version` baseline commit → GitHub Release soft-fail).
4. Runs `record_deploy_baseline.R` (full `DEPLOY_BASELINE.txt` with live `bundleId` / commit).
5. Pushes those commits to the checked-out branch (`contents: write`).
6. Soft-skips Mac OneDrive sync via `post_release_local_sync.sh` → `mac_post_release_sync_handoff.sh` (`PENDING_MAC_SYNC.env` + dispatch **Post-release Mac local sync**).

## Residual (Mac-owned)

- Coding clone + OneDrive physical backup sync remains **Mac-side** (launchd / self-hosted runner labels `self-hosted`, `macOS`, `theynow` / Cursor `agent worker start`).
- Prefer cloud tip-ship when the three `SHINYAPPS_*` secrets are configured; Mac remains preferred for local iteration and mandatory for post-release OneDrive catch-up.

## Out of scope / do not

- Do not paste secret values into PRs, chat, or YAML.
- Do not tip-ship paused PR #801 from this workflow setup work.
- Auto tip-ship on every `master` push is intentionally **not** enabled yet (manual `workflow_dispatch` only).
