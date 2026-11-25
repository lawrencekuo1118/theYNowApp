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
6. **Pushes** bump + baseline commits (fails the job if detached / push impossible). **Known issue:** direct push to protected `master` can fail **GH013** (`offline-checks` required) after a successful upload — follow-up should land bump/baseline via tip-ship PR + auto-merge so Git/live stay aligned.
7. Runs `record_github_release.sh` targeting the **pushed** SHA.
8. Soft-skips Mac OneDrive sync via `post_release_local_sync.sh` → handoff (`PENDING_MAC_SYNC.env` + **Post-release Mac local sync**).

## Residual (Mac-owned)

- Coding clone + OneDrive physical backup sync remains **Mac-side** (launchd / self-hosted runner labels `self-hosted`, `macOS`, `theynow` / Cursor `agent worker start`).
- Prefer cloud tip-ship when the three `SHINYAPPS_*` secrets are configured; Mac remains preferred for local iteration and mandatory for post-release OneDrive catch-up.

## Out of scope / do not

- Do not paste secret values into PRs, chat, or YAML.
- Do not tip-ship paused PR #801 from this workflow setup work.
- Auto tip-ship on every `master` push is intentionally **not** enabled yet (manual `workflow_dispatch` only).
