get_box_color <- function(industry, metric, value) {
  # 1. 🛑 新增防禦檢查：處理空值或未定義的產業
  if (is.null(industry) || industry == "" || length(industry) == 0) {
    return("black") # 預設顯示黑色
  }
  
  # 2. 處理數值無效值
  if (is.null(value) || is.na(value)) return("lime") 
  
  # 3. 檢查數值是否為負
  if (value < 0) return("red")
  
  # 4. 安全地檢查產業標準是否存在 (使用單括號 [ ] 配合 %in% 更安全)
  if (!(industry %in% names(industry_standards))) {
    return("black")
  }
  
  # 取得特定指標的標準
  std <- industry_standards[[industry]][[metric]]
  
  if (is.null(std) || length(std) != 2) {
    return("black")
  }
  
  bounds <- std
  
  # 5. 定義方向性並回傳顏色
  lower_is_better <- metric %in% c("opex_ratio", "eqt_multiplier")
  
  if (lower_is_better) {
    if (value < bounds[1]) return("aqua")
    if (value > bounds[2]) return("red")
    return("black")
  } else {
    if (value > bounds[2]) return("green")
    if (value < bounds[1]) return("red")
    return("black")
  }
}

#########
  
industry_standards <- list(
  sc.IC_Design = list(
    eqt_multiplier      = c(1.5, 2.5),
    rev_growth          = c(0, 15),
    gross_profit_margin = c(40, 70),
    opex_ratio          = c(30, 50),
    roa                 = c(10, 20),
    roe                 = c(15, 30),
    beta_avg = 1.35,
    rm_avg   = 8
  ),
  sc.Foundry = list(
    eqt_multiplier = c(1.5, 3.5),
    rev_growth = c(0, 10),
    gross_profit_margin = c(30, 55),
    opex_ratio = c(10, 25),
    roa = c(5, 12),
    roe = c(10, 20),
    beta_avg = 1.25,
    rm_avg   = 7.8
  ),
  sc.Packaging = list(
    eqt_multiplier = c(1.5, 2.5),
    rev_growth = c(0, 10),
    gross_profit_margin = c(15, 30),
    opex_ratio = c(20, 35),
    roa = c(5, 10),
    roe = c(8, 18),
    beta_avg = 1.15,
    rm_avg   = 7.5
  ),
  sc.Memory = list(
    eqt_multiplier = c(2.0, 4.0),
    rev_growth = c(0, 10),
    gross_profit_margin = c(20, 50),
    opex_ratio = c(15, 25),
    roa = c(-5, 15),
    roe = c(-10, 25),
    beta_avg = 1.4,
    rm_avg   = 8.2
  ),
  fn.Banking = list(
    eqt_multiplier = c(10, 20),
    rev_growth = c(0, 10),
    net_profit_margin = c(10, 30),
    opex_ratio = c(45, 60),
    beta_avg = 1.1,
    rm_avg   = 7.5
  ),
  fn.Investment_Banking = list(
    eqt_multiplier = c(15, 30),
    rev_growth = c(-5, 15),
    net_profit_margin = c(10, 25),
    opex_ratio = c(50, 70),
    beta_avg = 1.45,
    rm_avg   = 8.5
  ),
  fn.Insurance = list(
    eqt_multiplier = c(5, 15),
    rev_growth = c(0, 10),
    net_profit_margin = c(5, 15),
    opex_ratio = c(50, 65),
    beta_avg = 0.95,
    rm_avg   = 7.2
  ),
  auto.Vehicle_Manufacturing = list(
    eqt_multiplier = c(2.5, 5.0),
    rev_growth = c(-5, 10),
    net_profit_margin = c(3, 8),
    opex_ratio = c(10, 20),
    beta_avg = 1.2,
    rm_avg   = 8
  ),
  auto.Parts_Suppliers = list(
    eqt_multiplier = c(2.0, 4.0),
    rev_growth = c(-5, 12),
    net_profit_margin = c(4, 10),
    opex_ratio = c(12, 25),
    beta_avg = 1.15,
    rm_avg   = 7.8
  ),
  auto.EV_Startups = list(
    eqt_multiplier = c(1.2, 2.5),
    rev_growth = c(10, 50),
    net_profit_margin = c(-20, 5),
    opex_ratio = c(30, 60),
    beta_avg = 1.6,
    rm_avg   = 9
  ),
  fmcg.Food_Beverages = list(
    eqt_multiplier = c(1.5, 2.8),
    rev_growth = c(2, 8),
    gross_profit_margin = c(35, 50),
    opex_ratio = c(25, 35),
    net_profit_margin = c(8, 14),
    beta_avg = 0.75,
    rm_avg   = 7
  ),
  fmcg.Household_Personal = list(
    eqt_multiplier = c(1.8, 3.0),
    rev_growth = c(2, 10),
    gross_profit_margin = c(45, 60),
    opex_ratio = c(28, 38),
    net_profit_margin = c(10, 16),
    beta_avg = 0.8,
    rm_avg   = 7.2
  ),
  fmcg.Health_Beauty = list(
    eqt_multiplier = c(1.5, 2.5),
    rev_growth = c(5, 15),
    gross_profit_margin = c(50, 70),
    opex_ratio = c(30, 45),
    net_profit_margin = c(12, 20),
    beta_avg = 0.85,
    rm_avg   = 7.5
  ),
  hc.Healthcare_Services = list(
    rev_growth = c(0, 10),
    gross_profit_margin = c(30, 50),
    opex_ratio = c(40, 60),
    net_profit_margin = c(5, 12),
    roa = c(4, 10),
    roe = c(8, 18),
    beta_avg = 0.9,
    rm_avg   = 7.5
  ),
  hc.Pharma = list(
    rev_growth = c(0, 10),
    gross_profit_margin = c(60, 80),
    opex_ratio = c(30, 50),
    net_profit_margin = c(10, 20),
    roa = c(8, 15),
    roe = c(12, 25),
    beta_avg = 0.95,
    rm_avg   = 7.5
  ),
  hc.Medtech = list(
    rev_growth = c(0, 10),
    gross_profit_margin = c(55, 75),
    opex_ratio = c(25, 40),
    net_profit_margin = c(10, 22),
    roa = c(8, 18),
    roe = c(12, 28),
    beta_avg = 1.05,
    rm_avg   = 7.8
  ),
  hc.Biotech = list(
    rev_growth = c(0, 10),
    gross_profit_margin = c(50, 90),
    opex_ratio = c(50, 100),
    net_profit_margin = c(-100, 10),
    roa = c(-15, 8),
    roe = c(-30, 15),
    beta_avg = 1.5,
    rm_avg   = 8.5
  )
)
