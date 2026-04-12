# ==========================================
# default_config.R - 應用程式全域預設值設定檔
# 說明：統一管理 UI 與 Server 的初始數值，並動態綁定產業 KPI
# ==========================================

# 抓取預設產業的 KPI 數據 (取平均值作為預設參數)
ind_kpi <- industry_standards[[DEFAULT_IND]]

# 如果該產業有資料就用算出來的，沒有就給個保底安全值 (防呆)
default_beta <- if (!is.null(ind_kpi$beta_avg)) ind_kpi$beta_avg else 1.0
default_g    <- if (!is.null(ind_kpi$rev_growth)) mean(ind_kpi$rev_growth) else NA
default_rm   <- if (!is.null(ind_kpi$rm_avg)) ind_kpi$rm_avg else NA

# 預先計算 CAPM 股東權益成本 (r_e) 作為預設值
default_rf <- get_risk_free_rate()  # 改成呼叫爬蟲函數，自動抓最新值！

# CAPM 公式: Ke (或 re) = Rf + Beta * (Rm - Rf)
default_re <- default_rf + default_beta * (default_rm - default_rf)

# 預先定義統一的「預設永續成長率 (SGR)」
default_sgr <- round(get_risk_free_rate(), 2)

# 建立全域參數表
APP_DEFAULTS <- list(
  
  # --- 1. 基本設定 (共用) ---
  stock_code      = "AMZN",
  industry_choice = "ecr.Ecommerce_Retail",     # 預設產業
  years           = 5,                          # 預測年數
  
  # --- 2. DDM 股息折現模型 ---
  ddm_d0          = NA,              # 改為 NA，讓系統自動抓取財報真實股利
  ddm_g           = default_sgr,     # 綁定統一預設值        
  ddm_ke          = round(default_re, 2),
  
  # --- 3. Gordon 永續成長模型 ---
  dcf_mode        = "gordon",        
  g_growth_method = "fundamental",
  custom_g        = default_g,             # 綁定統一預設值
  sgr             = default_sgr,           # 綁定統一預設值
  wacc_gordon     = round(default_re, 2),  # 直接綁定算好的 default_re
  
  # --- 4. Two-Stage 二階段模型 ---
  yr_stage1       = 3,  
  g_stage1        = default_g,              # 綁定預設產業的營收成長率平均
  g_stage2        = default_sgr,            # 綁定統一預設值 (與 sgr 完全連動)
  wacc_stage1     = round(default_re, 2),   # 直接綁定算好的 default_re
  wacc_stage2     = round(default_re, 2),   # 直接綁定算好的 default_re
  
  use_calc_wacc   = TRUE,
  
  # --- 5. WACC 與 CAPM 參數 ---
  wacc_re         = round(default_re, 2),  # 直接綁定算好的 default_re
  wacc_rd         = 5.0,                   # 預設債務成本
  wacc_tax        = 20.0,                  # 預設稅率
  
  use_est_re      = TRUE,
  
  capm_rf         = round(default_rf, 2),   # 綁定即時爬蟲的 Rf
  capm_beta       = round(default_beta, 2), # 綁定產業的 Beta
  capm_rm         = round(default_rm, 2)    # 綁定產業的 Rm
)
