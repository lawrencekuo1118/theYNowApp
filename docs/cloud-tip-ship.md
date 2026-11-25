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

**No `TIP_SHIP_GH_PAT`.** Land uses the job `GITHUB_TOKEN` and Mac-parity direct push (`git push origin HEAD:<branch>`), same as original #806.

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
6. **Pushes** bump + baseline commits with job `GITHUB_TOKEN`: `git push origin HEAD:<branch>` (Mac parity / #806). Fails the job if detached / push impossible.
7. Runs `record_github_release.sh` targeting the **pushed** SHA.
8. Soft-skips Mac OneDrive sync via `post_release_local_sync.sh` → handoff (`PENDING_MAC_SYNC.env` + **Post-release Mac local sync**).

## GH013 residual (protected `master`)

Tip-ship #3 showed that a direct push to `master` is rejected (**GH013**) when branch protection / rulesets require status check **`offline-checks`**. A direct push never carries those PR checks, and the default Actions `GITHUB_TOKEN` does not fire follow-on `pull_request`/`push` workflows.

**One-time GitHub settings (not a new Actions secret):**

1. **Preferred for default `branch=master` tip-ship:** ruleset / branch protection — allow **`github-actions[bot]`** to bypass required status checks (or required pull requests) for tip-ship baseline pushes; **or**
2. Tip-ship a **non-protected** branch via workflow input `branch=…`, then open a normal human PR into `master` so `offline-checks` runs under your account.

Do **not** add `TIP_SHIP_GH_PAT` for this path.

## Residual (Mac-owned)

- Coding clone + OneDrive physical backup sync remains **Mac-side** (launchd / self-hosted runner labels `self-hosted`, `macOS`, `theynow` / Cursor `agent worker start`).
- Prefer cloud tip-ship when the three `SHINYAPPS_*` secrets are configured; Mac remains preferred for local iteration and mandatory for post-release OneDrive catch-up.

## Out of scope / do not

- Do not paste secret values into PRs, chat, or YAML.
- Do not tip-ship paused PR #801 from this workflow setup work.
- Auto tip-ship on every `master` push is intentionally **not** enabled yet (manual `workflow_dispatch` only).
