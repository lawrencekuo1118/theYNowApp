# The YNow App v12.0 — Backtest Logic Optimization

聚焦回測可信度：Point-in-Time 動態重建歷史合理價；圖上模式 A 為參數×歷史財報試算路徑（與持倉無關），模式 B 為情緒疊加曝險模擬。

## 回測設計重點
- Strategy A（圖）：現有 App 參數假設 × 歷史財報 PIT 綜合合理價（正規化），與持倉／曝險無關
- Exp_A／Trade_A：MOS 滯後倉位＋Great Filter（診斷用，亦為 B 的倉位基準）
- Strategy B：情緒僅能在 Exp_A 的 75%–125% 調整
- Historical Fair Value Timeline、MOS／FV 前瞻驗證、參數高原

## 執行

```r
shiny::runApp("app_12.0")
```

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
