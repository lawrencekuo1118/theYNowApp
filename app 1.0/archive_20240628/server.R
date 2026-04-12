if(!require(devtools)) install.packages("devtools")
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, ggrepel, Hmisc, jsonlite, 
  lubridate, magrittr, NLP, plotly, readr, 
  readxl, rio, reshape2, rvest, stats,
  stringr, xml2,
  tidyverse, shiny, shinydashboard, DT, shinyjs)

library(devtools); library(pacman); library(dplyr); library(tidyverse); 
library(ggplot2); library(ggrepel); library(Hmisc); library(jsonlite); library(plotly);
library(lubridate); library(magrittr); library(NLP);  library(readr); 
library(readxl); library(rio); library(reshape2); library(rvest); library(stats);
library(stringr);library(xml2);
library(shiny); library(shinydashboard); library(DT); library(shinycustomloader); library(shinyjs)

############################## setup
get.data <- function(no.stock){
  base_url <- "https://finance.yahoo.com/quote/"
  l_url <- list(
    paste0(base_url, no.stock),
    paste0(base_url, no.stock, "?p=", no.stock),
    paste0(base_url, no.stock, "/financials?p=", no.stock),
    paste0(base_url, no.stock, "/balance-sheet?p=", no.stock),
    paste0(base_url, no.stock, "/cash-flow?p=", no.stock)
  )
  return(lapply(l_url, read_html))  # HTML Content
}

vISbreakdowns <- c("Total Revenue", "Cost of Revenue", "Gross Profit", 
                   "Operating Expense", "Operating Income", "Total Expenses", "Net Income from Continuing & Discontinued Operation",
                   "EBIT", "EBITDA")

vBSbreakdowns <- c("Total Assets", "Total Liabilities Net Minority Interest", "Total Equity Gross Minority Interest", 
                   "Total Capitalization", "Common Stock Equity", "Capital Lease Obligations",
                   "Net Tangible Assets", "Invested Capital", "Tangible Book Value",
                   "Total Debt","Net Debt",
                   "Share Issued", "Ordinary Shares Number","Treasury Shares Number")

### Cross Analysis
fNum_tfrr <- function(df) {
  # Remove commas and convert to numeric, handling NAs
  df_cleaned <- as.data.frame(lapply(df, function(x) {
    # Remove commas and convert to numeric
    numeric_values <- as.numeric(gsub(",", "", x))
    
    # Replace NA values with NA in the result
    ifelse(is.na(x), NA, numeric_values)
  }), stringsAsFactors = TRUE)
  return(df_cleaned)
}

### Others
Loader1 <- function(x){ withLoader(x, type="html", loader="loader1") }

BoxColor <- function(num){ 
  if(is.na(num)){ return("black") }
  if(as.numeric(num) < 0){ 
    return("red") } 
  else{ return("black") }
} # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

############################## server
server <- function(input, output, session) {
  no.stock <- reactive({ input$sc })
  tmp <- reactive({
    req(input$sc)
    isolate(get.data(input$sc))
  })
  
  vPeriods <- c("Breakdown", "ttm", "Period1", "Period2", "Period3")
  observeEvent(input$sc, {
    updateRadioButtons(session, "periods", choices = vPeriods[2:5], selected = "ttm", inline = FALSE)
  })
  
  corp_name <- reactive({
    req(tmp())
    tmp()[[1]] %>%
      html_nodes(xpath = "//*[@id='quote-header-info']/div[2]/div[1]/div[1]/h1") %>% 
      html_text()
  })
  output$txt_corpname <- renderText({ corp_name() })
  
  ### FINANCE SUMMARY
  dFinanceSummary <- reactive({
    # overall breakdown
    df_financesummary <- data.frame()
    for(div in c(1:2)){
      for(tr in c(1:8)){
        for(i in c(1:2)){
          xpath_financesummary = paste0("//*[@id=\"quote-summary\"]/div[", div, "]/table/tbody/tr[", tr, "]/", "td[", i, "]")
          if(div==1){
            df_financesummary[tr, div+i-1] <- tmp()[[2]] %>% html_nodes(xpath=xpath_financesummary) %>% html_text
          }
          if(div==2){
            df_financesummary[tr, div+i] <- tmp()[[2]] %>% html_nodes(xpath=xpath_financesummary) %>% html_text
          }
        }
      }
    }
    colnames(df_financesummary) <- c("Name", "Value", "Name", "Value")
    return(df_financesummary)
  })
  
  output$tbFinanceSummary <- renderDataTable({ dFinanceSummary() })
  
  output$FS_download <- downloadHandler(
    filename = function() {
      paste0(as.character(no.stock()), "_financesummary_", as.character(Sys.Date()), ".csv")
    }, 
    content = function(file) {
      write.csv(dFinanceSummary(), file, row.names = FALSE)
    }
  )
  
  output$ibx_marketcap <- renderInfoBox({
    infoBox("Market Cap.",
            h3(dFinanceSummary()[1, 4], style = "font-size:150%;"),
            icon = icon("money-bill-trend-up"),
            color = "navy")
  })
  
  output$ibx_stockprice <- renderInfoBox({
    infoBox("Stock Price",
            h3(dFinanceSummary()[1, 2], style = "font-size:150%;"),
            icon = icon("money-bill"),
            color = "orange")
  })
  
  output$ibox_EPS <- renderInfoBox({
    infoBox("EPS",
            h3(dFinanceSummary()[4, 4], style = "font-size:150%;"),
            icon = icon("percent"),
            color = "green")
  })
  
  ### INCOME STATEMENT
  dIncomeStatement <- reactive({
    fIS_4Periods = function(ISno.col_tmp){
      xpath_tmp = paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[1]/div/div[", ISno.col_tmp, "]")
      col.name <- tmp()[[3]] %>% html_nodes(xpath=xpath_tmp) %>% html_text
      return(col.name)
    }
    
    fIS_Path2Txt <- function(xpath){
      result <- tmp()[[3]] %>% html_nodes(xpath = xpath) %>% html_text
      ifelse(length(result)==0, return(NA), return(result))
    }
    
    ISxpath = paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[2]/div[", c(1:50), "]/div[1]/div[1]")
    no.row <- as.data.frame(sapply(ISxpath, fIS_Path2Txt), stringsAsFactors = FALSE)[, 1] %>% na.omit() %>% length()
    
    # overall breakdown
    df_financials <- data.frame()
    for(ISdiv in c(1:no.row)){
      for(ISno.col in c(1:5)){
        xpath_financials = paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[2]/div[", ISdiv, "]/div[1]/div[", ISno.col, "]")
        df_financials[ISdiv, ISno.col] <- tmp()[[3]] %>% html_nodes(xpath=xpath_financials) %>% html_text
      }
    }
    colnames(df_financials) <- sapply(c(1:5), fIS_4Periods)
    return(df_financials)
  })
  
  output$tbIncomeStatement <- renderDataTable({ dIncomeStatement() })
  
  output$IS_download <- downloadHandler(
    filename = function(){ paste0(tolower(as.character(no.stock())), "_incomestatement_", as.character(Sys.Date()), ".csv") }, 
    content = function(x){ write.csv(dIncomeStatement(), x) }
  )
  
  # data charts
  actIncomeStatement <- reactive({
    dIncomeStatement()[c(1, which(colnames(dIncomeStatement()) == input$periods))] %>%
      filter(Breakdown %in% input$is_breakdown)
  })
  
  output$plt_revenue <- renderPlotly({
    dIncomeStatement() %>% filter(Breakdown %in% input$is_breakdown) %>%
      pivot_longer(
        cols = -Breakdown,
        names_to = "period",
        values_to = "amount") %>%
      ggplot(aes(x=fct_rev(factor(period)), y=amount)) +
      geom_col() +
      labs(title=paste("Plot of", input$is_breakdown, "by Periods"),
           x="period", y="amount")
  })
  
  # revenue growth
  rev.gth <- reactive({
    df_tmp <- fNum_tfrr(dIncomeStatement())
    idx <- which(dIncomeStatement()$Breakdown == "Total Revenue")
    rev.gth <- ((df_tmp[idx, 3] - df_tmp[idx, 4]) / df_tmp[idx, 4] + (df_tmp[idx, 4] - df_tmp[idx, 5]) / df_tmp[idx, 5]) / 2
    return(rev.gth)
  })
  
  
  output$vbx_rev.growth <- renderValueBox({
    valueBox(
      round(rev.gth()*100, 2), "3yr Revenue gth (%)", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(rev.gth())
    )
  })
  
  # Net Non Operating Interest Income Expense / EBIT
  NNOIIE.EBIT <- reactive({
    # Process the income statement data
    df_tmp <- fNum_tfrr(dIncomeStatement())
    
    # Find the indices for the breakdowns
    idx1 <- which(dIncomeStatement()$Breakdown == "Net Non Operating Interest Income Expense")
    idx2 <- which(dIncomeStatement()$Breakdown == "EBIT")
    
    # Calculate the ratio
    NNOIIE.EBIT <- df_tmp[idx1, "ttm"] / df_tmp[idx2, "ttm"]
    
    return(NNOIIE.EBIT)
  })
  
  output$vbx_NNOIIE.EBIT <- renderValueBox({
    valueBox(
      round(NNOIIE.EBIT()*100, 2), "NNOIIE/EBIT (%)", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(NNOIIE.EBIT())
    )
  })
  
  #Net Profit Margin Ratio: Net Income from Continuing Operation Net Minority Interest / Total Revenue
  N.profit.margin <- reactive({
    df_tmp <- fNum_tfrr(dIncomeStatement())
    idx1 <- which(dIncomeStatement()$Breakdown == "Net Income from Continuing Operation Net Minority Interest")
    idx2 <- which(dIncomeStatement()$Breakdown == "Total Revenue")
    
    if (length(idx1) == 0 || length(idx2) == 0) {
      return(NA)  # Handle case where indices are not found
    }
    
    if (is.na(df_tmp[idx1, "ttm"]) || is.na(df_tmp[idx2, "ttm"]) || df_tmp[idx2, "ttm"] == 0) {
      return(NA)  # Handle case where values are missing or denominator is zero
    }
    
    N.profit.margin <- df_tmp[idx1, "ttm"] / df_tmp[idx2, "ttm"]
    return(N.profit.margin)
  })
  
  output$vbx_N.profit.margin <- renderValueBox({
    valueBox(
      round(N.profit.margin()*100, 2), "N.Profit Margin (%)", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(N.profit.margin())
    )
  })
  
  # Gross Profit Margin Ratio: Gross Profit / Total Revenue
  G.profit.margin <- reactive({
    # Process the income statement data
    df_tmp <- fNum_tfrr(dIncomeStatement())
    
    # Find the indices for the breakdowns
    idx1 <- which(dIncomeStatement()$Breakdown == "Gross Profit")
    idx2 <- which(dIncomeStatement()$Breakdown == "Total Revenue")
    
    # Calculate the ratio
    G.profit.margin <- df_tmp[idx1, "ttm"] / df_tmp[idx2, "ttm"]
    
    return(G.profit.margin)
  })
  
  output$vbx_G.profit.margin <- renderValueBox({
    valueBox(
      round(G.profit.margin()*100, 2), "G.Profit Margin (%)", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(G.profit.margin())
    )
  })
  
  # Gross Profit Growth
  G.profit.gth <- reactive({
    # Process the income statement data
    df_tmp <- fNum_tfrr(dIncomeStatement())
    
    # Find the index for the breakdown
    idx <- which(dIncomeStatement()$Breakdown == "Gross Profit")
    
    # Calculate the growth
    G.profit.gth <- ((df_tmp[idx, 3] - df_tmp[idx, 4]) / df_tmp[idx, 4]) +
      ((df_tmp[idx, 4] - df_tmp[idx, 5]) / df_tmp[idx, 5]) / 2
    
    return(G.profit.gth)
  })
  
  output$vbx_G.profit.gth <- renderValueBox({
    valueBox(
      round(G.profit.gth()*100, 2), "3yr G.Profit gth (%)", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(G.profit.gth())
    )
  })
  
  ### BALANCE SHEET
  dBalanceSheet <- reactive({
    fBS_4Periods = function(BSno.col_tmp){
      xpath_tmp = paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[1]/div/div[", BSno.col_tmp, "]")
      col.name <- tmp()[[4]] %>% html_nodes(xpath=xpath_tmp) %>% html_text
      return(col.name)
    }
    
    fBS_Path2Txt <- function(xpath){
      result <- tmp()[[4]] %>% html_nodes(xpath = xpath) %>% html_text
      ifelse(length(result)==0, return(NA), return(result))
    }
    
    BSxpath = paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[2]/div[", c(1:50), "]/div[1]/div[1]")
    BStitle <- as.data.frame(sapply(BSxpath, fBS_Path2Txt), stringsAsFactors = FALSE)[, 1] %>% na.omit()
    no.row <- BStitle %>% length()
    
    # overall breakdown
    df_balancesheet <- data.frame()
    for(BSdiv in c(1:no.row)){
      df_balancesheet[BSdiv, 1] <- BStitle[BSdiv] #第一欄標題
      for(BSno.col in c(2:4)){
        xpath_balancesheet = paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[2]/div[", BSdiv, "]/div[1]/div[", BSno.col, "]/span")
        htxt <- tmp()[[4]] %>% html_nodes(xpath=xpath_balancesheet) %>% html_text
        ifelse(length(htxt)==0, 
               df_balancesheet[BSdiv, BSno.col] <- NA, 
               df_balancesheet[BSdiv, BSno.col] <- htxt)
      }
    }
    colnames(df_balancesheet) <- sapply(c(1:4), fBS_4Periods)
    df_balancesheet <- cbind(df_balancesheet[, 1], df_balancesheet[, 2], df_balancesheet[, 2:ncol(df_balancesheet)])
    colnames(df_balancesheet)[1:2] <- c(fBS_4Periods(1), "ttm")
    return(df_balancesheet)
  })
  
  output$tbBalanceSheet <- renderDataTable({ dBalanceSheet() })
  
  output$BS_download <- downloadHandler(
    filename = function(){ paste0(tolower(as.character(no.stock())), "_balancesheet_", as.character(Sys.Date()), ".csv") }, 
    content = function(x){ write.csv(dBalanceSheet(), x) }
  )
  
  # data charts
  output$pieCapitalStructure <- renderPlot({ 
    dBalanceSheet()[1:3, ] %>%
      select(Breakdown, ttm) %>%
      ggplot(aes(x="", y=ttm, fill=Breakdown)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      geom_label(aes(label = Breakdown),
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE) +
      theme_void()
  })
  
  actBalanceSheet <- reactive({
    dBalanceSheet()[c(1, which(colnames(dBalanceSheet()) == input$periods))] %>%
      filter(Breakdown %in% input$bs_breakdown)
  })
  
  output$pieCapitalStructure2 <- renderPlot({
    actBalanceSheet() %>% 
      ggplot(aes(x="", y=.[, 2], fill=Breakdown)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      geom_label(aes(label = Breakdown),
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE) +
      theme_void()
  })
  
  #Total Assets / Total Equity Gross Minority Interest
  eqt.mutiplier <- reactive({
    df_tmp <- fNum_tfrr(dBalanceSheet())
    idx1 <- which(dBalanceSheet()$Breakdown == "Total Assets")
    idx2 <- which(dBalanceSheet()$Breakdown == "Total Equity Gross Minority Interest")
    eqt.mutiplier <- df_tmp[idx1, "ttm"] / df_tmp[idx2, "ttm"]
    return(eqt.mutiplier)
  })
  
  output$vbx_eqt.mutiplier <- renderValueBox({
    valueBox(
      round(eqt.mutiplier(), 2), "Equity Multiplier", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(eqt.mutiplier())
    )
  })
  
  #Debt Ratio: Total Debt / Total Assests
  dbt.ratio <- reactive({
    df_tmp <- fNum_tfrr(dBalanceSheet())
    idx1 <- which(dBalanceSheet()$Breakdown == "Total Debt")
    idx2 <- which(dBalanceSheet()$Breakdown == "Total Assets")
    dbt.ratio <- df_tmp[idx1, "ttm"] / df_tmp[idx2, "ttm"]
  })
  
  output$vbx_dbt.ratio <- renderValueBox({
    valueBox(
      round(dbt.ratio(), 2), "Debt rto.", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(dbt.ratio())
    )
  })
  
  ### CASH FLOW
  dCashFlow <- reactive({
    fCF_4Periods = function(CFno.col_tmp){
      xpath_tmp = paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[1]/div/div[", CFno.col_tmp, "]")
      col.name <- tmp()[[5]] %>% html_nodes(xpath=xpath_tmp) %>% html_text
      return(col.name)
    }
    
    fCF_Path2Txt <- function(xpath) {
      result <- tmp()[[5]] %>% html_nodes(xpath = xpath) %>% html_text
      ifelse(length(result) == 0, return(NA), return(result))
    }
    
    CFxpath <- paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[2]/div[", c(1:50), "]/div[1]/div[1]/div[1]/span")
    CFtitle <- as.data.frame(sapply(CFxpath, fCF_Path2Txt), stringsAsFactors = FALSE)[, 1] %>% na.omit()
    no.row <- length(CFtitle)
    
    # Check if CFtitle is empty, and return an empty data frame if it is
    if (no.row == 0) {
      return(data.frame())
    }
    
    # Overall breakdown
    df_cashflow <- data.frame()
    
    for (CFdiv in c(1:no.row)) {
      df_cashflow[CFdiv, 1] <- CFtitle[CFdiv]  # 第一列標題
      for (CFno.col in c(2:5)) {
        xpath_cashflow <- paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[2]/div[", CFdiv, "]/div[1]/div[", CFno.col, "]/span")
        htxt <- length(tmp()[[5]] %>% html_nodes(xpath = xpath_cashflow) %>% html_text)
        if (htxt == 0) {
          df_cashflow[CFdiv, CFno.col] <- NA  # Replace with NA for missing values
        } else {
          df_cashflow[CFdiv, CFno.col] <- tmp()[[5]] %>% html_nodes(xpath = xpath_cashflow) %>% html_text
        }
      }
    }
    
    colnames(df_cashflow) <- sapply(c(1:5), fCF_4Periods)
    return(df_cashflow)
  })
  
  output$tbCashFlow <- renderDataTable({ dCashFlow() })
  
  output$CF_download <- downloadHandler(
    filename = function(){ paste0(tolower(as.character(no.stock())), "_cashflow_", as.character(Sys.Date()), ".csv") }, 
    content = function(x){ write.csv(dCashFlow(), x) }
  )
  
  opcf.gth <- reactive({
    g.opcf_df_tmp <- fNum_tfrr(dCashFlow())
    g.opcf_idx <- which(dCashFlow()$Breakdown == "Operating Cash Flow")
    opcf.gth <- ((g.opcf_df_tmp[g.opcf_idx, 3]-g.opcf_df_tmp[g.opcf_idx, 4])/g.opcf_df_tmp[g.opcf_idx, 4]) + ((g.opcf_df_tmp[g.opcf_idx, 4]-g.opcf_df_tmp[g.opcf_idx, 5])/g.opcf_df_tmp[g.opcf_idx, 5]) / 2
    return(opcf.gth)
  })
  
  output$vbx_OPCF.growth <- renderValueBox({
    valueBox(
      round(opcf.gth()*100, 2), "3yr Operating CF gth (%)", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(opcf.gth())
    )
  })
  
  ivcf.gth <- reactive({
    g.ivcf_df_tmp <- fNum_tfrr(dCashFlow())
    g.ivcf_idx <- which(dCashFlow()$Breakdown == "Investing Cash Flow")
    ivcf.gth <- ((g.ivcf_df_tmp[g.ivcf_idx, 3]-g.ivcf_df_tmp[g.ivcf_idx, 4])/g.ivcf_df_tmp[g.ivcf_idx, 4]) + ((g.ivcf_df_tmp[g.ivcf_idx, 4]-g.ivcf_df_tmp[g.ivcf_idx, 5])/g.ivcf_df_tmp[g.ivcf_idx, 5]) / 2
    return(ivcf.gth)
  })
  
  output$vbx_IVCF.growth <- renderValueBox({
    valueBox(
      round(ivcf.gth()*100, 2), "3yr Investing CF gth (%)", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(ivcf.gth())
    )
  })
  
  fncf.gth <- reactive({
    g.fncf_df_tmp <- fNum_tfrr(dCashFlow())
    g.fncf_idx <- which(dCashFlow()$Breakdown == "Financing Cash Flow")
    fncf.gth <- ((g.fncf_df_tmp[g.fncf_idx, 3]-g.fncf_df_tmp[g.fncf_idx, 4])/g.fncf_df_tmp[g.fncf_idx, 4]) + ((g.fncf_df_tmp[g.fncf_idx, 4]-g.fncf_df_tmp[g.fncf_idx, 5])/g.fncf_df_tmp[g.fncf_idx, 5]) / 2
    return(fncf.gth)
  })
  
  output$vbx_FNCF.growth <- renderValueBox({
    valueBox(
      round(fncf.gth()*100, 2), "3yr Financing CF gth (%)", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(fncf.gth())
    )
  })
  
  # data charts
  actCashFlow <- reactive({
    dCashFlow()[c(1, which(colnames(dCashFlow()) == input$periods))]
  })
  
  output$plt_3cashflow <- renderPlot({
    df_tmp <- dCashFlow()[1:3, 1:5]
    df_tmp <- df_tmp %>% pivot_longer(
      cols = -Breakdown,
      names_to = "periods",
      values_to = "amount"
    )
    ggplot(df_tmp, aes(x = as.factor(periods), y = amount, group = Breakdown)) +
      geom_line(aes(color = Breakdown)) +
      labs(title = "Three Cash Flow Comparison", x = "periods", y = "amount")
  })
  
  ### Cross Analysis
  #ROA: Net Income from Continuing Operation Net Minority Interest	+ Net Interest Income *(1 - Tax Rate for Calcs) / mean(Total Assets 1, Total Assets 2)
  ROA <- reactive({
    ROA_df_tmp1 <- fNum_tfrr(dBalanceSheet()); ROA_df_tmp2 <- fNum_tfrr(dIncomeStatement())
    ROA_idx1 <- which(dIncomeStatement()$Breakdown == "Net Income from Continuing Operation Net Minority Interest")
    ROA_idx2 <- which(dIncomeStatement()$Breakdown == "Net Interest Income")
    ROA_idx3 <- which(dIncomeStatement()$Breakdown == "Tax Rate for Calcs")
    ROA_idx4 <- which(dBalanceSheet()$Breakdown == "Total Assets")
    roa <- (ROA_df_tmp2[ROA_idx1, "ttm"] + ROA_df_tmp2[ROA_idx2, "ttm"] * (1 - ROA_df_tmp2[ROA_idx3, "ttm"])) / ROA_df_tmp1[ROA_idx4, "ttm"]
    return(roa)
  })
  
  output$vbx_ROA <- renderValueBox({
    valueBox(
      round(ROA()*100, 2), "ROA", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(ROA())
    )
  })
  
  #ROE: Net Income from Continuing Operation Net Minority Interest / mean(Total Equity Gross Minority Interest 1, Total Equity Gross Minority Interest 2)
  ROE <- reactive({
    ROE_df_tmp1 <- fNum_tfrr(dBalanceSheet()); ROE_df_tmp2 <- fNum_tfrr(dIncomeStatement())
    ROE_idx1 <- which(dIncomeStatement()$Breakdown == "Net Income from Continuing Operation Net Minority Interest")
    ROE_idx2 <- which(dBalanceSheet()$Breakdown == "Total Equity Gross Minority Interest")
    roe <- ROE_df_tmp2[ROE_idx1, "ttm"] / ROE_df_tmp1[ROE_idx2, "ttm"]
    return(roe)
  })
  
  output$vbx_ROE <- renderValueBox({
    valueBox(
      round(ROE()*100, 2), "ROE", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(ROE())
    )
  })
  
  #Asset Turnover Ratio: 全年營收 / 期末總資產 
  ast.turnover <- reactive({
    AST_df_tmp1 <- fNum_tfrr(dBalanceSheet()); AST_df_tmp2 <- fNum_tfrr(dIncomeStatement())
    AST_idx1 <- which(dIncomeStatement()$Breakdown == "Total Revenue")
    AST_idx2 <- which(dBalanceSheet()$Breakdown == "Total Assets")
    ast.turnover <- AST_df_tmp2[AST_idx1, "ttm"] / AST_df_tmp1[AST_idx2, "ttm"]
    return(ast.turnover)
  })
  
  output$vbx_ast.turnover <- renderValueBox({
    valueBox(
      round(ast.turnover(), 2), "Asset Turnover", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(ast.turnover())
    )
  })
  
  #營業現金流/稅後淨利
  opcf.NI <- reactive({
    OPCFNI_df_tmp1 <- fNum_tfrr(dCashFlow()); OPCFNI_df_tmp2 <- fNum_tfrr(dIncomeStatement())
    OPCFNI_idx1 <- which(dIncomeStatement()$Breakdown == "Net Income from Continuing Operation Net Minority Interest")
    OPCFNI_idx2 <- which(dCashFlow()$Breakdown == "Operating Cash Flow")
    opcf.NI <- OPCFNI_df_tmp2[OPCFNI_idx1, "ttm"] / OPCFNI_df_tmp1[OPCFNI_idx2, "ttm"]
    return(opcf.NI)
  })
  
  output$vbx_opcf.NI <- renderValueBox({
    valueBox(
      round(opcf.NI(), 2), "Operating CF/N.Income", icon = icon("thumbs-down", lib = "glyphicon"),
      color = BoxColor(opcf.NI())
    )
  })
  
  ### Fraud Warnings
  output$vbx_fcf <- renderValueBox({
    df_tmp1 <- fNum_tfrr(dCashFlow()); #view(df_tmp1)
    idx1 <- which(dCashFlow()$Breakdown == "Free Cash Flow")
    fcf <- df_tmp2[idx1, "ttm"]
  })
  
  output$nofreecashflow <- renderText({
    Wng_df_tmp1 <- fNum_tfrr(dCashFlow()); #view(Wng_df_tmp1)
    Wng_idx1 <- which(dCashFlow()$Breakdown == "Free Cash Flow")
    fcf <- Wng_df_tmp1[Wng_idx1, "ttm"]; #print("Warning 01: ", fcf)
    if(fcf <= 0){ print("Warning 01: no free cash flow") }
  })
  
  output$vbx_opcf <- renderValueBox({
    df_tmp2 <- fNum_tfrr(dCashFlow()); #view(df_tmp2)
    idx2 <- which(dCashFlow()$Breakdown == "Operating Cash Flow")
    opcf <- df_tmp2[idx2, "ttm"]
  })
  
  output$nooperatingcashflow <- renderText({
    Wng_df_tmp2 <- fNum_tfrr(dCashFlow()); #view(Wng_df_tmp2)
    Wng_idx2 <- which(dCashFlow()$Breakdown == "Operating Cash Flow")
    ocf <- Wng_df_tmp2[Wng_idx2, "ttm"]; #print("Warning 02: ", ocf)
    if(ocf <= 0){ print("Warning 02: no operating cash flow") }
  })
  
  output$notdoingbusiness <- renderText({
    if(rev.gth() >= opcf.gth()){ print("Warning 03: not doing business") }
  })
  
  output$notgettingcashback <- renderText({
    if(opcf.NI() < 0.80){ print("Warning 04: not getting cash back") }
  })
  
  ### Others
  values <- reactiveValues()
  values$recentsearch <- c()
  observeEvent(input$sc, {
    # Add the current search term to the search history
    values$recentsearch <- c(values$recentsearch, corp_name())
    # Output the updated search history
    output$recentsearch <- renderText({
      paste(values$recentsearch, collapse = ", ")
    })
  })
  
  output$today <- renderText({
    format(Sys.Date(), "%Y/%m/%d")
  })
}
