# theYNowApp v11.0（雲端相容）

Taiwan stock fundamental analysis Shiny app（雲端版：yfinance，無 Chromote）。

## v11.0

- **Get Started**：模型建議卡片 + DCF 核心參數（永續成長率）集中設定
- **Snapshot**：即時參數／公式一覽，含時間戳下載
- **Finance Summary**：分組卡片網格（Price／Volume／Valuation／Dividend），數值完整保留
- **KPI 版面**：φ⁻¹ 等比縮小，一列五個靠左排列
- DCF Overview 預設 Simple Mode；WACC 輸入列重排（E／D／T 在上）
- 回測績效說明文字不再被 valueBox 遮蓋；主搜尋下拉僅在輸入時顯示

## Run

```r
# from the repository root
shiny::runApp("app_11.0")
```

Requires R packages from `global.R` / `setup.R`, and Python deps in `requirements.txt`.

## Cloud notes

- Financials via **yfinance**（shinyapps.io 不需 Chrome）
- `requirements.txt` / reticulate for cloud Python
- KPI helpers use multi-alias row matching for yfinance naming
- Backtest Zone：依公司基本面自動帶參數

Live: https://hopesmasher1118.shinyapps.io/TheYNowApp/
