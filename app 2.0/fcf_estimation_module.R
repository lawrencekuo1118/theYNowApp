fcf_estimation_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("🔍 自由現金流預估（FCF - DCF 模型）"),
    plotOutput(ns("fcf_plot"), height = "300px"),  # 圖表
    tableOutput(ns("fcf_table")),           # 表格
    downloadButton(ns("download_fcf"), "下載 FCF 預測結果")
  )
}


fcf_estimation_module_server <- function(id, d_income_statement, d_cash_flow, d_balance_sheet) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    extract_dcf_parameters <- function(df_is, df_cf, df_bs = NULL) {
      get <- function(metric, df) select_clean_metric_row(df, metric)
      
      revenue <- get("Total Revenue", df_is)
      op_income <- get("Operating Income", df_is)
      income_tax <- get("Income Tax Expense", df_is)
      pre_tax <- get("Earnings Before Tax", df_is)
      d_and_a <- get("Depreciation & Amortization", df_cf)
      capex <- get("Capital Expenditure", df_cf)
      ca <- get("Total Current Assets", df_bs)
      cash <- get("Cash And Cash Equivalents", df_bs)
      cl <- get("Total Current Liabilities", df_bs)
      std <- get("Short Term Debt", df_bs)
      
      delta_assets <- ca[1] - ca[2] - (cash[1] - cash[2])
      delta_liab <- cl[1] - cl[2] - ifelse(length(std) >= 2, std[1] - std[2], 0)
      delta_nwc <- delta_assets - delta_liab
      delta_rev <- revenue[1] - revenue[2]
      
      list(
        revenue_start = revenue[1],
        revenue_growth = get_avg_growth(revenue) / 100,
        ebit_margin = mean(op_income[1:2] / revenue[1:2], na.rm = TRUE),
        tax_rate = mean(income_tax[1:2] / pre_tax[1:2], na.rm = TRUE),
        da_rate = mean(d_and_a[1:2] / revenue[1:2], na.rm = TRUE),
        capex_rate = mean(abs(capex[1:2]) / revenue[1:2], na.rm = TRUE),
        nwc_rate = if (!is.na(delta_rev) && delta_rev != 0) delta_nwc / delta_rev else 0
      )
    }
    
    fcf_projection_dcf_style <- function(
    revenue_start, years = 5,
    revenue_growth = 0.08,
    ebit_margin = 0.15,
    tax_rate = 0.20,
    da_rate = 0.05,
    capex_rate = 0.07,
    nwc_rate = 0.10
    ) {
      revenue <- numeric(years)
      ebit <- nopat <- da <- capex <- delta_nwc <- fcf <- numeric(years)
      revenue[1] <- revenue_start
      for (t in 1:years) {
        if (t > 1) revenue[t] <- revenue[t - 1] * (1 + revenue_growth)
        ebit[t] <- revenue[t] * ebit_margin
        nopat[t] <- ebit[t] * (1 - tax_rate)
        da[t] <- revenue[t] * da_rate
        capex[t] <- revenue[t] * capex_rate
        delta_rev <- if (t == 1) 0 else revenue[t] - revenue[t - 1]
        delta_nwc[t] <- delta_rev * nwc_rate
        fcf[t] <- nopat[t] + da[t] - capex[t] - delta_nwc[t]
      }
      
      data.frame(
        Year = seq_len(years) + as.numeric(format(Sys.Date(), "%Y")) - 1,
        Revenue = round(revenue, 2),
        EBIT = round(ebit, 2),
        NOPAT = round(nopat, 2),
        Dep_Amort = round(da, 2),
        CapEx = round(capex, 2),
        Delta_NWC = round(delta_nwc, 2),
        FCF = round(fcf, 2)
      )
    }
    
    # 📊 表格與圖表
    fcf_df <- reactive({
      p <- scenario_params()
      if (is.null(p)) return(NULL)
      fcf_projection_dcf_style(
        revenue_start = p$revenue_start,
        years = 5,
        revenue_growth = p$revenue_growth,
        ebit_margin = p$ebit_margin,
        tax_rate = p$tax_rate,
        da_rate = p$da_rate,
        capex_rate = p$capex_rate,
        nwc_rate = p$nwc_rate
      )
    })
    
    output$fcf_table <- renderTable({
      df <- fcf_df()
      if (is.null(df)) return(data.frame(提醒 = "⚠️ 無法產生預測（資料不足）"))
      df
    })
  })
}
