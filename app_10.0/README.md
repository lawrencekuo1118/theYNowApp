# theYNowApp v10.0（雲端相容）

與 v9.0 功能相同，但針對 **shinyapps.io** 調整：

- 財報以 **yfinance API** 為主（無需 Chrome），並後處理對齊 v9.0 顯示：
  - 合併 `ttm_*` → `TTM` 欄
  - 數值格式 `742.78B` / `-2.47B`（與 Yahoo HTML 相同）
  - 列名／列序接近 Yahoo；Expand 使用 collapsed 白名單
- Selenium 僅本機可選後備（正式結果以 yfinance 為準）
- 本機可選 `./.ynow_venv` → `~/.venv`（`ln -s ~/.venv .ynow_venv`）

雲端部署依賴 `requirements.txt` / `py_require`（無需 Selenium）。
