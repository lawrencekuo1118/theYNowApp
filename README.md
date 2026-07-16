# theYNowApp v10.1（雲端相容）

Taiwan stock fundamental analysis Shiny app（雲端版：yfinance，無 Chromote）。

## v10.1

- **側邊欄「推薦」標記**：依 Model Selector（配息／FCF／產業）自動在 DD-Model、DCF-Model、P/B-Asset 旁顯示紅色「推薦」徽章
- 與 Dashboard 估值導航同一套規則（DCF／DDM／雙模型／P/B）
- **Ticker 下拉預選**：主搜尋框（`sc`）以原生 datalist 提供熱門清單 + Yahoo 即時建議；側邊欄搜尋框維持原樣

## Run

```r
# from the repository root
shiny::runApp()
```

Requires R packages from `global.R` / `setup.R`, and Python deps in `requirements.txt`.

## Cloud notes

- Financials via **yfinance**（shinyapps.io 不需 Chrome）
- `requirements.txt` / reticulate for cloud Python
- KPI helpers use multi-alias row matching for yfinance naming
- Backtest Zone：依公司基本面自動帶參數

Live: https://hopesmasher1118.shinyapps.io/TheYNowApp/
