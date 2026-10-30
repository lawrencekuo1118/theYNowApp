# 台股搜尋停留 yfinance 的修正佐證

案例：2330、7772；新增支援 7772-TWO、7772.TWO、7772，以及上市 2330-TW／2330.TW／2330。

## 已重現的問題

正式站 v27.59 的 2330 搜尋，120 秒後仍顯示「正在讀取 Summary（yfinance）」，連線仍在、沒有前端 JavaScript error。這是搜尋流程未收尾，並非可據此判定瀏覽器或整個 worker 崩潰。

1. 搜尋以 `later` 分階段執行，callback 沒有保留 session domain。資料失敗時 `showNotification()` 找不到 session，發生 `attempt to apply non-function`，外層又把錯誤吞掉，進度視窗未關閉。本機停用 Python 可重現；修正後同一失敗情境正常通知並清理進度。
2. 正式站 reticulate 1.41.0.1 出現 managed Python 暫存路徑不存在及 `object 'oldlibpath' not found`。新版 1.47.0 將 oldlibpath 初始化移到條件外；已實際驗證 Python 3.11 初始化及兩檔 Summary 擷取。
3. Summary 使用 Yahoo 多重 info／fast_info／history 備援，沒有整體等待上限；失敗的空 Summary 又可能被 fast cache 保留，重試不能立即恢復。
4. 共用 Postgres cache 把 serialized raw vector 直接綁為第四個 SQL 參數，正式站回報 `Parameter 4 does not have length 1`。現改為單一 bytea 參數 `list(raw)`。
5. 搜尋只接受點號板別，`7772-TWO` 不能一致解析；公司全稱亦延後至 Yahoo 與其他公司資訊取得後才顯示。完整畫面驗證另顯示，股權／公司事件的額外查詢會阻塞核心財報階段。

## 修正

- 延後的搜尋 callback 在原 session domain 與 isolated reactive scope 中執行；已關閉的 session 取消 callback；非預期失敗亦關閉進度並清除 busy。
- 部署 preflight 要求 reticulate >=1.47.0；managed Python 固定 3.11，避免舊版初始化錯誤。初始化失敗不再重複嘗試未設定的 Python。
- 整個 Summary HTTP／備援鏈預設限時 20 秒（不包含首次 Python 安裝、後續公司資訊與財報階段）。逾時顯示來源未提供資料，不代填財務值。每個 worker 最多一個尚未完成的 Summary 背景請求，避免重試堆積；逾時後未完成的 HTTP 自行結束，不能宣稱已取消網路請求。
- 空 Summary 不快取；成功結果仍使用原 15 分鐘 fast tier。
- 台股公司中文全稱先由既有官方股票清單帶入；2330 為「台灣積體電路製造股份有限公司」，7772 為「耀穎光電股份有限公司」。板別依清單解析，不因使用者輸入連字號就更換市場。
- 數字代號的 `-TW`／`-TWO` 轉為 `.TW`／`.TWO`，支援 Search、US→TW 自動切換判斷、typeahead 與歷史價格入口；美股 `BRK.B`→`BRK-B` 維持原行為。
- 已知上市股票的來源失敗不再誤試上櫃後綴；公司資訊階段顯示獨立進度文字。
- Summary 一次取得的 profile 同步用於公司簡介與產業對應，避免再次請求 Yahoo。股權、重要日期、董事改選及 CB 額外查詢延至核心搜尋完成且使用者開啟公司／Board-CB 頁面時執行；logo 配色亦等待核心搜尋完成。分階段切換預留 Shiny flush，先顯示已取得的結果。

## 驗證

- `test_tw_search_session.R`：實際 later callback 的 domain、reactive read、錯誤通知／清理、已關閉 session、代號別名、官方全稱、typeahead、失敗後重試與成功快取。
- `test_market_cache_binary.R`：模擬 RPostgres 單列參數約束，驗證 bytea 綁定及序列化還原。
- `test_summary_deadline.py`：成功、來源錯誤、逾時、限制未完成工作數、請求完成後恢復。
- 既有 9 項回歸腳本通過：market profile、recent search、ticker ranking、typeahead UI、cache tiers、TPEx 財報備援、會計參數語意、參數缺值、估值公式。
- 實際 reticulate 1.47.0／Python 3.11 已取得兩檔公司名稱及 20 列 Summary。這項驗證不等於保證 Yahoo 隨時可用或財報資料必定齊全。

操作失敗時不捏造財務數據；外部服務失敗、興櫃資料不足或正式上游科目缺漏仍須如實顯示。
