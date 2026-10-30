# 財務參數來源與模型引用稽核

日期：2026-10-30。活動線：`app_27.0/`。本報告的判斷對象為程式實際引用與可重現測試資料；不代表已逐家公司核對所有最新原始申報文件。

## 結果與驗證範圍

已修正跨幣別 WACC、DCF 執行與情境模式不同步、跨 session 市場與匯率污染，以及會計科目與公式參數錯配。缺少必要財報資訊時保留 NA；Full 提供人工補填導引；Lite 直接套用有依據的參考建議，並標示「Lite 已套用假設數值」。無可靠建議的財報值仍須人工補填。

測試使用 US/TW × en/zh-TW × Full/Lite × 7 種身份狀態 × USD/TWD 分析幣別，共 112 個基礎組合。身份狀態為 guest、member、ops_admin、sysadmin、sysadmin_guest、sysadmin_signed_in、sysadmin_omniscient。這些是引擎測試中的狀態組合，並非逐一登入七個正式帳號的端到端驗證。

| 驗證 | 修正前失敗 | 修正後失敗 | 證據 |
|---|---:|---:|---|
| WACC 同幣別資本權重，112 組 | 56 | 0 | `evidence/wacc-{before,after}.csv` |
| DCF 執行與情境一致，含隱藏欄位延遲，224 組 | 56 | 0 | `evidence/wacc-{before,after}-dcf.csv` |
| Rf 在另一 session 切換市場後仍正確，224 組 | 112 | 0 | `evidence/wacc-{before,after}-rf.csv` |
| 28 項會計語意 × 112 組，3,136 項 | 未對舊版全量執行 | 0 | `evidence/semantics-after.csv` |
| 16 項針對性測試腳本 | — | 0 | `evidence/suite.json` 與各 `*-ship.log` |

獨立 MockShinySession 驗證 market/currency/FX 隔離通過。實際瀏覽器確認最新 Shiny 首頁可連線，JavaScript errors 與 Shiny output errors 均為空。Full/Lite 缺值處理、Lite 寫入、人工改值後移除舊假設標示，以及舊 ticker 欄位值等待新資料回傳的保護均有 Shiny 測試。

## 市場來源與共用模型

台美市場共用公式、科目解析、股數與幣別換算函式；市場設定保留不同來源。US Rf 使用美國 10 年期公債 Yahoo `^TNX`；TW 使用 TPEx 公債殖利率曲線 10 年期。財報主要經 Yahoo/yfinance 取得；上櫃 Yahoo 資料缺漏時沿用既有 TPEx 財務簡報補充路徑。稅率先解析同期間 Tax Provision / Pretax Income；無法取得時市場假設為 US 21%、TW 20%，不是公司實際有效稅率的宣稱。

語言切換只調整文案。Full/Lite 使用同一財報與核心模型；Lite 會採推薦模式及直接套用有依據的參考值，Full 可保留人工選擇。因此操作假設可能不同，但不能另建一套會計來源。角色控制功能可見度，不應更換科目語意。市場、分析幣別與 USD/TWD 匯率改為 session domain 儲存，避免不同使用者互相污染。

## 公式參數逐項對應

| 模型／參數 | 正確定義與本次處理 | 禁止替代／缺值行為 |
|---|---|---|
| FCFF NOPAT | 同財年 EBIT 或 Operating Income × (1−T) | Net Income 不能替代 EBIT/NOPAT |
| FCFF D&A、CapEx、ΔNWC | 對齊 IS/CF 財年，再按同年資料計算 | 不以最新 D&A 填所有歷年；缺值不當零 |
| FCFE | FCFF − after-tax interest + net borrowing，以 Ke 折現 | 不以 WACC 替代 Ke；缺少利息或有其他資本類別時拒絕不完整轉換 |
| Rd | gross accrual Interest Expense / interest-bearing Debt | 不用 Net Interest Expense 或 Cash Interest Paid |
| WACC | Ke×MV/(MV+D) + Rd×(1−T)×D/(MV+D)，兩項權重同幣別 | 不用 book equity 替代 MV；來源失敗清除舊結果 |
| 多類資本 WACC | 有其他權益請求權時需人工完整 WACC | 不將二類資本公式當作多類資本公式 |
| DCF 股價橋接 | (EV + Cash − Debt − Other claims market value) / quote-class Shares | 不把其他權益帳面值當市值；未知不自動零 |
| DDM D0 | 明確 trailing annual paid DPS，對齊報價幣別與股權單位 | 不用 forward rate、Dividend Yield、公司總股利 |
| DDM 人工歷史平均工具 | 普通股股利；總股利僅能扣除已知特別股股利；換至報價幣別 | 歸屬不明或匯率缺失不產生數值 |
| RI B0 | Common Stock Equity / quote-class Shares | 不直接用含其他權益的總數 |
| RI ROE、payout | common earnings / common equity；common dividends / common earnings | 不把所有股東盈餘或特別股股利當普通股資料 |
| P/B | 選定 BVPS/TBVPS/NAVPS，同報價單位 | 未知 intangibles 不視為零；不偷換所選基礎 |
| NAV | common net assets per quote-class share | 不用未扣除特別股等請求權的總淨資產 |
| P/E | 明確報告 DilutedEPS，次選 BasicEPS，否則 common NI / weighted-average shares | 不用期末 BS shares 算 EPS；跨幣別不直接引用財報 EPS |
| EV multiples | 各自 FCFF/EBIT/EBITDA/Revenue × 同定義倍數，再做 equity bridge | 不把 levered Yahoo FCF 當 FCFF；分析金額先換成報價幣別 |
| SOTP | 至少兩個可辨識部門營收與明確 EV/Sales 倍數，各部門加總後 equity bridge | 不接受單一部門、重名、非數字或非正值；避免現金重複計入 |
| Operating IC | Assets − Cash − Nonoperating Assets − (Liabilities − InterestDebt) | 不用 Total Assets 或未排除非營運資產的近似值；必要項缺失須補填 |

共同規則：自動選用可比較財報期間；無交集則 NA。Shares 使用報價級距並保留 ADR 換算檢查；每股盈餘採加權平均股數，估值分母採報價級距股數，兩者不可混用。DCF 手動修改 shared shares 後亦實際用於分母。

## 可重現數值佐證

US 測試：股權市值 2,000 USD、有息負債 500 USD、Ke 10%、Rd 5%、T 21%，WACC 應為 8.79%。若負債誤用 16,000 TWD 與 USD 市值混算，結果為 4.6222%。修正後先統一幣別。

TW 測試：股權市值 96,000 TWD、有息負債 32,000 TWD、Ke 10%、Rd 5%、T 20%，WACC 應為 8.5%。誤把負債換成 1,000 USD 卻保留 TWD 市值會得到 9.9381%。修正後所有組合均為 8.5%。

會計反例刻意讓 raw Yahoo FCF=999，而依同期間營運資料重建 FCFF=88，確認引擎使用 88；另外令 NI、EBIT、common NI、common equity、total equity、forward/trailing DPS、gross/net/cash interest 各不相同，避免同值測試掩蓋綁錯科目。詳見 `test_parameter_semantics.R`。

## 缺值、建議與假設揭露

`parameter_completeness.R` 為七種模型建立條件式必要欄位清單。零與缺值分開處理；已確認零值可用，未知值不可補零。FCFE 需 Ke；兩階段需各階段假設；P/B 依選定基礎；RI 依 ROE 路徑；SOTP 需部門資料。

Full 顯示建議但不自動填入。Lite 對有限數值的參考建議直接寫入，包含已計算折現率及系統預設估算假設；每個套用欄位顯示假設標示、數值、來源解釋。這些紀錄接入 PDF warnings。切換 ticker 清除紀錄；使用者改值後不再將舊數值列為已套用假設。財報事實（如 NOPAT、Cash、Debt）沒有可靠數值時維持 NA，顯示補填通知及導向欄位按鈕。

## PDF 與重現方式

`test_report_download.R` 使用六種公司資料形態 × en/zh-TW × Full/Lite，24 種報告組合，包含缺 CF、NAV 與無可用 Fair Value；HTML 與 Chromium PDF 渲染已執行。渲染日誌仍含既有數學排版提示及測試圖片的 localhost 404，故不能將「通過」解讀為每頁視覺皆已人工驗收。

```bash
export YNOW_DEBUG_SKIP_PY=1
Rscript app_27.0/tests/test_parameter_semantics.R
Rscript app_27.0/tests/test_parameter_completeness.R
cd app_27.0/tests
Rscript test_valuation_formulas.R
Rscript test_phase1_formula_independence.R
# 需 Chromium、pandoc、rmarkdown/pagedown
YNOW_REPORT_TEST_PDF=1 Rscript test_report_download.R
```

## 證據邊界與仍需人工判斷

測試證明列出的科目語意、錯誤反例及模式組合符合本次斷言，不是對所有發行人的上游資料品質作無條件保證。未逐一登入正式帳號驗證授權後畫面，也未逐家公司對帳 MOPS/SEC 原件。自動跨幣別主要支援 USD/TWD；其他幣別沒有有效換算時應維持不可用。CFO 重建 FCFF 仍依現有模型對利息分類的假設；IFRS 發行人的實際現金流分類、非營運資產與多類資本結構需使用者審閱。預測成長率、估值倍數、terminal g 與人工輸入均是估值假設，並非已報告財務事實。

部署紀錄另見 `scripts/DEPLOY_BASELINE.txt`。Cloud 不具 Mac coding clone/OneDrive 路徑；完成部署後仍需同步該實體備份。
