# ==========================================
# global.R - 應用程式進入點與全域設定
# ==========================================

# 1. 載入全域設定與套件 (原本的 global 2.0.R 已經處理好這部分)
source("global 2.0.R", encoding = "UTF-8")

# 2. 載入資料抓取與爬蟲模組
source("setup 5.0.R", encoding = "UTF-8")
source("search_module 4.0.R", encoding = "UTF-8")

# 3. 載入產業標準清單與防呆顏色設定
source("industry_standards.R", encoding = "UTF-8")

# 4. 載入自定義的 Shiny Server 模組
source("kpi_module.R", encoding = "UTF-8")
source("fcf_projection_module.R", encoding = "UTF-8")
source("ddm_module.R", encoding = "UTF-8")

# 載入我們剛寫好的全域預設值設定檔
source("default_config.R", encoding = "UTF-8")
