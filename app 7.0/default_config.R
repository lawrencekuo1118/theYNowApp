# ==========================================
# default_config.R - 應用程式全域預設值設定檔
# 說明：統一管理 UI 與 Server 的初始數值，並動態綁定產業 KPI
# ==========================================

# 1. 定義初始預設產業
DEFAULT_IND <- "ecr.Ecommerce_Retail"

# 2. 抓取預設產業的 KPI 數據 (取平均值作為預設參數)
ind_kpi <- industry_standards[[DEFAULT_IND]]

# 如果該產業有資料就用算出來的，沒有就給個保底安全值 (防呆)
default_beta <- if (!is.null(ind_kpi$beta_avg)) ind_kpi$beta_avg else 1.0
default_rm   <- if (!is.null(ind_kpi$rm_avg)) ind_kpi$rm_avg else 8.0
default_g    <- if (!is.null(ind_kpi$rev_growth)) mean(ind_kpi$rev_growth) else 5

# 🟢 預先計算 CAPM 股東權益成本 (r_e) 作為預設值
# default_rf <- 4.2  <-- 將原本這行刪掉或註解掉
default_rf <- get_risk_free_rate()  # 🌟 改成呼叫爬蟲函數，自動抓最新值！

# CAPM 公式: Ke (或 re) = Rf + Beta * (Rm - Rf)
default_re <- default_rf + default_beta * (default_rm - default_rf)

# 3. 建立全域參數表
APP_DEFAULTS <- list(
  
  # --- 1. 基本設定 (DCF 共用) ---
  stock_code = "AMZN",
  industry_choice = DEFAULT_IND,     # 預設產業
  years           = 5,               # 預測年數
  
  # --- 2. DDM 股息折現模型 ---
  ddm_d0          = 5,               
  ddm_g           = 3,               
  ddm_ke  = round(default_rf + default_beta * (default_rm - default_rf), 2),
  
  # --- 3. Gordon 永續成長模型 ---
  dcf_mode        = "gordon",        
  g_growth_method = "cagr",
  g_gordon        = default_g,       # 🟢 綁定預設產業的營收成長率平均
  custom_g        = 5,               
  wacc_gordon     = 10,              
  
  # --- 4. Two-Stage 二階段模型 ---
  yr_stage1       = 3,  
  g_stage1        = default_g,       # 🟢 綁定預設產業的營收成長率平均
  g_stage2        = 3,               # 通膨率2-3%
  wacc_stage1     = 10,              
  wacc_stage2     = 9,               
  
  # --- 5. FCF 進階歷史推算參數 ---
  var_capex_rate  = NA,              
  var_nwc_rate    = NA,
  use_hist_capex  = TRUE,
  use_hist_nwc    = TRUE, 
  
  # --- 6. CAPM & WACC 折現率參數 ---
  capm_rf         = default_rf,      # 🟢 共用同一組 Rf
  capm_beta       = default_beta,    # 🟢 綁定預設產業的 Beta 值
  capm_rm         = default_rm,      # 🟢 綁定預設產業的 Rm 值
  wacc_re = round(default_rf + default_beta * (default_rm - default_rf), 2),
  wacc_rd         = 5.0,             
  wacc_tax        = 20.0, 
  use_est_re      = TRUE,      
  use_calc_wacc   = TRUE            
)
