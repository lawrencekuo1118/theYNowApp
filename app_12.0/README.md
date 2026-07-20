# The YNow App v12.0 — Backtest Logic Optimization

聚焦回測可信度：Point-in-Time 動態重建歷史合理價。上方圖比較 PIT 合理價、歷史股價（情緒疊加價值）與大盤；下方圖為策略淨值（曝險 A／情緒疊加策略）。

## 回測設計重點
- 上方圖：參數 × 歷史財報 PIT 合理價（隨模型選擇）＋歷史股價表現＋大盤
- Exp_A／Trade_A：MOS 滯後倉位＋Great Filter（下方策略圖；亦為情緒策略基準）
- 情緒策略：僅能在 Exp_A 的 75%–125% 調整
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
- 圖上模式 A＝參數×歷史財報合理價路徑；Exp_A／B＝曝險模擬（情緒僅能在 Exp_A 的 75%–125% 調整）

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
