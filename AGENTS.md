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

- Live app line: **`app_16.0/`** (see `scripts/DEPLOY_BASELINE.txt` for deployed baseline).
- Prefer existing labels in `ynow_ui.R` / module UI before inventing new terms.
- CapEx spike smoothing UI: **暴衝倍數閾值**、**均值年數**、**週期**（勿用「周期」）。

### i18n after each feature (required)

**每次特定功能開發完成後**，自動補齊雙語，勿等使用者再說「翻譯」：

1. 新 UI 文案同時提供 **en-US** 與 **zh-TW**（台灣繁體；勿用簡體／港式）。
2. 字串必須進 `app_16.0/ui_locale.R` 的 `.UI_STRINGS$en` / `.UI_STRINGS$zh-TW`（同一 key），再經 `ui_str`、`.push_ui_locale`、市場→locale 推送，以及前端 `applyUiLocale` / `ynowUiLocale` 套用。
3. **禁止**只硬編碼單一語言；僅財經專有名詞維持正式英文（WACC、FCFF、MOS…）可例外。詳見 `.cursor/rules/i18n-after-feature.mdc`。

## Testing

- Run targeted tests under `app_16.0/tests/` when changing valuation or FCF logic.
- Non-trivial UI changes: manual/browser verification when the environment supports it.

## Cursor Cloud specific instructions

- **No extra DEMO / walkthrough verification** in Cursor Cloud agent runs: do **not** produce demo videos, UI walkthrough recordings, mockup screenshots, or similar “proof artifacts” unless the user explicitly asks for them.
- Prefer automated tests under `app_16.0/tests/` plus targeted shell／R／Python checks as evidence.
- Still ship per auto-deploy rules when `app_16.0/` behavior changes are verified; docs／AGENTS／rules-only → commit + push only (no shinyapps deploy, no version +0.01).

## Git & deploy

See `.cursor/rules/auto-deploy-after-optimize.mdc` and `.cursor/rules/dual-workspace-sync.mdc` for ship workflow and workspace sync.

**Auto deploy：** 每次在活動線 `app_16.0/` **完成開發並驗證後**，一律自動 commit → push → `Rscript scripts/deploy_app_16.R` → 更新 `DEPLOY_BASELINE.txt`，無需等候使用者再說「部署／推送」。僅文件／規則／未完成 WIP 或使用者明確要求不部署時略過。

**i18n before ship：** 功能收尾時先完成 en-US + zh-TW（`ui_locale.R` + locale push）。**僅規則／AGENTS／文件**變更 → commit + push，**不** shinyapps 部署、**不**版號 +0.01。

**Version bump：** 每次 merge／ship 將 UI／header 等顯示版號 **+0.01**（如 `v15.01`、`v15.02`）；目錄可維持 `app_16.0/`，僅在使用者要求整階 **+1** 時才改名。詳見 `.cursor/rules/version-bump-on-merge.mdc`。
