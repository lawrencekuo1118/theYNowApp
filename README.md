# theYNowApp

Taiwan stock fundamental analysis Shiny app（雲端版：yfinance，無 Chromote）。

## 維護方式

本機僅維護此路徑，**每個迭代版本一個資料夾**：

`/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp`

- 目前版本：`app_10.0/`（v10.1）
- 歷史版本：`app 3.0` … `app_9.0`（本機封存；GitHub Releases 另有標籤）

## 執行目前版本

```r
shiny::runApp("app_10.0")
```

## v10.1 重點

- 側邊欄「推薦」標記（DDM／DCF／P/B）
- 主搜尋框 Ticker 預選建議
- 永續成長率：Macro／Fundamental／Lifecycle
- 搜尋後自動 WACC；DCF 圖含歷史／折現模式
- Backtest Zone 版面重整（執行面板 + 三步驟參數分頁）

## Cloud

Live: https://hopesmasher1118.shinyapps.io/TheYNowApp/
