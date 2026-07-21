# The YNow App v12.0 — Backtest Logic Optimization

聚焦回測可信度：Point-in-Time 動態重建歷史合理價；**淨值圖**比較兩大模式的**策略淨值**（倉位×報酬），合理價另見 Fair Value 時間軸。

## 回測設計重點
- **純基本面價值 (Trade_A)**：MOS 滯後＋Great Filter → Exp_A → 策略淨值（淨值圖橘線）
- **情緒波動價值 (Trade_B)**：Exp_B = Exp_A × 情緒乘數（75%–125%）→ 策略淨值（淨值圖藍線；嵌套於 A）
- **評價模型**：算合理價／MOS（驅動 Exp_A）；**不**畫在淨值圖
- **Model_A**：合理價指數，供參數高原；HFV Timeline 用美元合理價點
- Historical Fair Value Timeline、MOS／FV 前瞻驗證、參數高原

## 執行

```r
shiny::runApp("app_12.0")
```

進入點為 `app.R`（唯一）。UI／Server 分別在 `ynow_ui.R`／`ynow_server.R`，勿在根目錄再放 `ui.R`／`server.R`，否則 shinyapps 可能出現「No UI defined」。

## V12 核心（不做估值倉庫）

查詢時即時抓取歷史財報／股價，在 **Current Session** 動態重建各再平衡日的：

- DCF / DDM / RI / P/B Fair Value
- MOS、訊號可解釋明細
- 兩模式策略淨值（Trade_A／Trade_B）與 Buy&Hold／SPY 比較

結果不永久落庫。

## 回測驗證面板

| 面板 | 回答的問題 |
|------|------------|
| Historical Fair Value Timeline | 當年模型能否辨識低估／高估？ |
| Alpha Dashboard | 是否優於 Buy & Hold？（CAGR／Sharpe／α） |
| Exposure History | A 是否長期不在場？（現金拖累） |
| B&H Gap | 輸給 B&H 的原因拆解 |
| MOS 有效性 | MOS 愈高前瞻報酬是否愈好？ |
| Fair Value Edge | 模型價 > 市價時，1Y／3Y／5Y 是否較佳？ |
| 參數高原 | WACC／SGR／年數／VG 是否穩健？ |

## 技術約束

- 維持 R Shiny；可部署 shinyapps.io
- 禁止 historical fair value warehouse
- 不新增與回測無關的估值模型或資料源
