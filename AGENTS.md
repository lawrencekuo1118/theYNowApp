# AGENTS.md — theYNowApp

Guidance for Cursor Agents working in this repository.

## Language & terminology (required)

All **user-facing copy**, **commit/PR descriptions to the user**, and **in-app UI strings** must follow:

1. **Formal English** for standard finance / valuation terms (keep the English term when it is the industry norm).
2. **Taiwan Traditional Chinese (zh-TW)** for explanatory prose and labels—not Mainland (`zh-CN`) or Hong Kong wording.
3. **No Simplified Chinese** in UI or agent replies.

### Prefer (English or Taiwan usage)

| Topic | Use |
|-------|-----|
| Discount rate | WACC、Ke（CAPM） |
| Cash flows | FCFF、FCFE、FCF、CapEx、ΔNWC、NOPAT |
| Terminal value | Terminal Value、終值、永續成長率 g、SGR |
| Models | Gordon 模型、Two-Stage DCF、DDM、RI |
| UI / engineering | 預設、參數、資料、使用者、勾選、程式、程式碼、週期、閾值、軟體、網路、資訊、部署、推送 |
| Finance TW | 營收、財報、折現率、自由現金流、資本支出、營運資金、安全邊際 MOS、加權平均資本成本 |

### Avoid (Mainland / simplified / non-TW)

`默认` `参数` `数据` `用户` `勾选` `周期` `阈值` `软件` `网络` `信息` `门限` `质量` `账户` `报表` `视频` `内存` 以及任何简体字。

When unsure: use the **English term** + brief Taiwan Chinese gloss on first mention.

### App copy conventions

- Live app line: **`app_15.0/`** (see `scripts/DEPLOY_BASELINE.txt` for deployed baseline).
- Prefer existing labels in `ynow_ui.R` / module UI before inventing new terms.
- CapEx spike smoothing UI: **暴衝倍數閾值**、**均值年數**、**週期**（勿用「周期」）。

## Testing

- Run targeted tests under `app_15.0/tests/` when changing valuation or FCF logic.
- Non-trivial UI changes: manual/browser verification when the environment supports it.

## Git & deploy

See `.cursor/rules/auto-deploy-after-optimize.mdc` and `.cursor/rules/dual-workspace-sync.mdc` for ship workflow and workspace sync.

**Auto deploy：** 每次在活動線 `app_15.0/` **完成開發並驗證後**，一律自動 commit → push → `Rscript scripts/deploy_app_15.R` → 更新 `DEPLOY_BASELINE.txt`，無需等候使用者再說「部署／推送」。僅文件／規則／未完成 WIP 或使用者明確要求不部署時略過。

**Version bump：** 每次 merge／ship 將 UI／header 等顯示版號 **+0.01**（如 `v15.01`、`v15.02`）；目錄可維持 `app_15.0/`，僅在使用者要求整階 **+1** 時才改名。詳見 `.cursor/rules/version-bump-on-merge.mdc`。
