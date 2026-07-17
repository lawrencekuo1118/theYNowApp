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
  auto.Automotive_EV = list(
    eqt_multiplier      = c(3.0, 8.0),
    rev_growth          = c(5, 20),
    gross_profit_margin = c(12, 25),
    opex_ratio          = c(10, 18),
    roa                 = c(2, 8),
    roe                 = c(10, 20),
    beta_avg = 1.40,
    rm_avg   = 9.0
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
  ec.Hardware = list(
    eqt_multiplier      = c(2.5, 6.0),
    rev_growth          = c(2, 12),
    gross_profit_margin = c(25, 45),
    opex_ratio          = c(10, 25),
    roa                 = c(8, 18),
    roe                 = c(25, 50),
    beta_avg = 1.10,
    rm_avg   = 8.0
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
  lxg.Luxury_Fashion = list(
    eqt_multiplier      = c(1.5, 2.5),
    rev_growth          = c(5, 12),
    gross_profit_margin = c(60, 75),
    opex_ratio          = c(30, 45),
    roa                 = c(10, 18),
    roe                 = c(20, 35),
    beta_avg = 1.05,
    rm_avg   = 8.0
  ),
  ecr.Ecommerce_Retail = list(
    eqt_multiplier      = c(2.5, 4.5),
    rev_growth          = c(5, 15),
    gross_profit_margin = c(20, 35), # 零售業毛利較低
    opex_ratio          = c(15, 30),
    roa                 = c(3, 8),
    roe                 = c(15, 25),
    beta_avg = 0.95,
    rm_avg   = 7.5
  ),
  saas.SaaS_Cloud = list(
    eqt_multiplier      = c(2.0, 5.0),
    rev_growth          = c(10, 25),
    gross_profit_margin = c(70, 85),
    opex_ratio          = c(40, 60),
    roa                 = c(5, 15),
    roe                 = c(15, 35),
    beta_avg = 1.20,
    rm_avg   = 9.0
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
  ),
  en.Energy_OilGas = list(
    eqt_multiplier      = c(1.8, 3.0),
    rev_growth          = c(-5, 15), # 波動大
    gross_profit_margin = c(20, 40),
    opex_ratio          = c(5, 15),
    roa                 = c(5, 12),
    roe                 = c(12, 25),
    beta_avg = 1.10,
    rm_avg   = 8.5
  )
)

# 🎨 KPI 顏色防呆判定 (終極防呆版)
get_box_color <- function(industry_choice, metric_name, val) {
  # 1. 終極防呆：檢查參數是否為空 (這行能解決 attempt to select less than one element 錯誤)
  if (is.null(industry_choice) || length(industry_choice) == 0 || industry_choice == "") return("black")
  if (is.null(metric_name) || length(metric_name) == 0) return("black")
  if (is.na(val) || is.null(val)) return("black")
  
  # 2. 確保標準清單中有這個產業
  if (!(industry_choice %in% names(industry_standards))) return("black")
  
  # 3. 抓取該產業的該項指標標準區間 (例如 c(30, 50))
  std <- industry_standards[[industry_choice]][[metric_name]]
  if (is.null(std) || length(std) != 2) return("black")
  
  # 4. 判斷是否落在標準區間內
  if (val >= std[1] && val <= std[2]) {
    return("black") # 在標準內亮綠燈
  } else if (val < std[1]) {
    return("red")   # 低於標準亮紅燈
  } else {
    return("blue")  # 高於標準 (表現優異) 亮藍色
  }
}
