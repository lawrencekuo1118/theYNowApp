# Lite 智慧分析自動套用預設：修正與佐證

## 行為

Lite 直接套用可用的預測假設並自動計算；不再跳出「前往補填」或要求切換 Full。欄位、智慧分析畫面及既有 PDF 假設清單揭露「Lite 已套用假設數值」。Full 保留來源缺漏及人工確認流程。

這次使用現有 APP_DEFAULTS，不另建一套財報來源。例：RI 預期 ROE 15%、預期配息率 40%、預測 5 年；RI 永續成長率的備援改接中央 SGR 預設。可取得的當期來源與折現率仍優先，僅在缺漏時使用明示假設。折現率備援是估值假設，不宣稱是當期 CAPM 或實際 WACC。

必要的實際財報科目，例如普通股 B0、股數、現金、營收，仍須來自原有正確來源，不能以不相干科目或無根據的零替代。來源確實未提供時，Lite 顯示來源缺漏及無法產出該模型結果的原因，不顯示人工補填按鈕，也不捏造結果。

## 根因與修正

1. RI 的成長率補值查找不存在的 `APP_DEFAULTS$ri_g`；改採既有 `sgr`。
2. Lite 只追蹤是否曾送出預設值。延後來源更新若把欄位清成 NA，就不再重套。現追蹤瀏覽器是否已回傳，回傳後再次缺值會重套；等待回傳期間不重複送出。補值前再次確認欄位仍為缺值，避免去抖後的舊清單覆寫剛取得的財報值或人工值。
3. 自動計算簽章未涵蓋 RI 配息率／ROE 等欄位，預設值回傳後未必重算。現納入模型參數；RI 等待必要預測值皆有效才執行。
4. DCF 就緒條件仍強制要求自動 calculated WACC，即使 Lite 已明示採用假設折現率。Full 維持此要求，Lite 允許已套用的假設輸入。
5. Lite 移除人工補填通知，智慧分析直接呈現假設清單。真正來源缺漏顯示獨立說明。
6. RI 公式拒絕缺漏 Payout，防止 Full 或直接呼叫把 NA 默認成零；Lite 先補上明示的預期配息率，再送入同一公式。
7. 已整合 master 的可見分頁優先更新（PR #580）。其既有測試比對跨行註解而失敗，改比對實際隱藏分頁 guard，未更改排程行為。

## 驗證

- `test_parameter_completeness.R`：Full 不寫入、Lite 自動套用、雙語、缺漏財報維持 NA、延後清空後重新套用、假設清單，正式 Gordon／Two-Stage readiness gate，以及實際 RI 公式計算。B0=100、ROE=15%、Payout=40% 時第一年股利為 6；NA Payout 回報 error。
- `test_valuation_formulas.R`：既有估值公式回歸通過。
- `test_parameter_source_modes.R`：既有模式矩陣回歸通過（角色為引擎標籤測試，並非所有帳號實際登入）。
- `test_parameter_semantics.R`：3,136 項會計參數語意檢查，失敗 0。
- Lite UI 與分頁排程測試通過。
- 實際本機瀏覽器：訪客、Lite、en-US、TW、`7772-TWO`，無人工填值，60 秒完成。RI NT$71.09、P/B NT$125.40、MOS -34.0%；揭露 ROE 15%／Payout 40% 等已套用假設，未顯示補填通知，無 JavaScript／Shiny output error。數字是當次來源與假設下的驗證結果，不保證後續報價相同。

原始結果見 `evidence/`，瀏覽器步驟見 `verify-browser.py`。本機 R 的 Debian plotly 套件曾帶入不適用瀏覽器的 typedarray，已還原為上游 plotly.R 的 polyfill 後驗證；此為驗證環境修復，沒有更改產品 JavaScript。另一輪遇到 Yahoo 未提供 Summary，App 如實顯示來源失敗，沒有捏造價格或估值。

## 部署

- 正式版：v27.61；appId 10907657，bundle **12765906**，API 確認 running，靜態 build metadata 亦為 v27.61。
- 修正已由 [PR #581](https://github.com/lawrencekuo1118/theYNowApp/pull/581) 合併 master（`9778eccd`），必要 CI／audit 皆通過，原始紀錄見 `evidence/pr-581.json`。
- 部署紀錄已由 [PR #582](https://github.com/lawrencekuo1118/theYNowApp/pull/582) 合併 master（`e7ddeb78`）；基準記錄原提交 `8fc86923`。
- 實際上傳程式版本：`b07d83e5fb9093ffb6df947d4444bbe9aec5e3d2`（版號提交）。[GitHub Release v27.61](https://github.com/lawrencekuo1118/theYNowApp/releases/tag/v27.61) 指向此版本；部署基準見 `scripts/DEPLOY_BASELINE.txt`。
- 正式站實測：訪客、Lite、en-US、TW、7772-TWO；沒有人工補填，110 秒完成（含新版首次啟動等待）。RI **NT$71.09**、P/B **NT$122.72**、MOS **-34.0%**；B0 38.35、Ke 9.74%、g 2.03%，預期 ROE 15%／Payout 40% 有明確假設揭露。補填通知未出現，無 JavaScript／Shiny output error；原始結果 `evidence/production-en.jsonl`。
- 正式站 zh-TW Lite 亦實測通過（75 秒），顯示「Lite 已套用假設數值」，無補填通知、JavaScript／Shiny output error；見 `evidence/production-zh-TW.jsonl`。
- 同一正式站 session 由 en-US 切換 zh-TW，四張估值卡數值逐項相同（主模型、副模型、市價、MOS），證明語言切換不改估值；見 `evidence/production-locale-stability.jsonl`。不同查詢當下可用的歷史價格／倍數資料可能不同，因此獨立 session 的副模型數字不作跨時點一致性斷言。
- 實際財報來源缺漏仍不造值：本案例 TPEx 補入 IS／BS；Cash Flow 未提供則維持缺漏，由可用的 RI／P/B 提供結果。
- 已執行 `post_release_local_sync.sh`；Cloud 未掛載 Mac／OneDrive，腳本 soft-skip。**本機備份尚未完成**，須在 Mac coding clone 執行同步及 `check-od-sync.sh`。
