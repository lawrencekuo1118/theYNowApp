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

############################## ui
ui <- dashboardPage( 
  skin = "black",
  
  dashboardHeader(
    title = "The YNow App",
    titleWidth = 250), #"calc(100% - 250)"
  
  dashboardSidebar(
    width = 250, # must same with titleWidth
    collapsed = TRUE,
    
    column(width = 12,
           sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."), hr()
    ),
    column(width = 12,
           sidebarMenu(
             menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
             menuItem("Calculator", tabName = "calculator", icon = icon("equalizer", lib = "glyphicon"), 
                      badgeLabel = "new", badgeColor = "green"),
             menuItem("About", tabName = "about",  icon = icon("info-sign", lib = "glyphicon"))
           ), hr()
    ), 
    column(width = 12,
           radioButtons("periods", h5("Period Options"), choices = ""), hr(),
           h5("Recent Search:"),
           textOutput("recentsearch"), hr()
    ), 
    column(width = 12,
           textOutput("today"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('.main-header .logo { font-weight: bold }'))),
    
    fluidRow(
      column(width = 12,
             titlePanel(h2("Let's find some frauds from financial reports!"))
      ),
      column(width = 8,
             titlePanel(h5("a lawrence kuo shiny app")),
             textInput("sc", "Stock Code", value = "AAPL"), # set stock default
             submitButton("Search", icon("search"))
      ),
      column(width = 4,
             textOutput("txt_corpname"),
             #Loader1(uiOutput("imagesUI"))
      )), br(),
    
    fluidRow(
      infoBoxOutput("ibx_marketcap"),
      infoBoxOutput("ibx_stockprice"),
      infoBoxOutput("ibox_EPS")
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              tabBox(title = "Financial Reports", width = "auto",
                     tabPanel("Finance Summary",
                              p("this section import Finance Summaries from Yahoo Finance"),
                              Loader1(dataTableOutput("tbFinanceSummary")),
                              downloadButton('FS_download',"Download the data")),
                     
                     tabPanel("Income Statement",
                              p("this section import Income Statements from Yahoo Finance"),
                              fluidRow( 
                                column(width = 6, 
                                       selectizeInput("is_breakdown", h5("Breaksdown Options"),                                 
                                                      choices = vISbreakdowns, selected = "Total Revenue",
                                                      multiple = FALSE),
                                       Loader1(plotlyOutput("plt_revenue", height = 300))
                                )
                              ),
                              Loader1(dataTableOutput("tbIncomeStatement")),
                              downloadButton('IS_download',"Download the data")),
                     
                     tabPanel("Balance Sheet",
                              p("this section import Balance Sheets from Yahoo Finance"),
                              fluidRow( 
                                column(width = 6, 
                                       selectizeInput("bs_breakdown", h5("Breakdown Options"),                                 
                                                      choices = vBSbreakdowns, selected = c("Total Assets", "Total Debt"),
                                                      multiple = TRUE),
                                       Loader1(plotOutput("pieCapitalStructure2", height = 300))
                                ),
                                column(width = 6,
                                       Loader1(plotOutput("pieCapitalStructure", height = 300)))
                              ),
                              Loader1(dataTableOutput("tbBalanceSheet")),
                              downloadButton('BS_download',"Download the data")),
                     
                     tabPanel("Cash Flow",
                              p("this section import Cash Flow datas from Yahoo Finance"),
                              Loader1(plotOutput("plt_3cashflow")),
                              Loader1(dataTableOutput("tbCashFlow")),
                              downloadButton('CF_download',"Download the data"))
              ),
              
              tabBox(title = "Performance", width = "auto",
                     tabPanel("KPI by Sheet",
                              fluidRow(
                                column(width = 12,
                                       h4("Balance Sheet KPI"),
                                       valueBoxOutput('vbx_eqt.mutiplier')
                                ),
                                column(width = 12,
                                       h4("Income Statement KPI"),
                                       valueBoxOutput('vbx_rev.growth'),
                                       valueBoxOutput('vbx_G.profit.gth'),
                                       valueBoxOutput('vbx_NNOIIE.EBIT'),
                                       valueBoxOutput('vbx_G.profit.margin'),
                                       valueBoxOutput('vbx_N.profit.margin')
                                ),
                                column(width = 12,
                                       h4("Cash FLow Statement KPI"),
                                       valueBoxOutput('vbx_OPCF.growth'),
                                       valueBoxOutput('vbx_IVCF.growth'),
                                       valueBoxOutput('vbx_FNCF.growth')
                                )
                              )
                     ),
                     
                     tabPanel("Crossover KPI",
                              fluidRow(
                                column(width = 12,
                                       valueBoxOutput('vbx_ROA'),
                                       valueBoxOutput('vbx_ROE'),
                                       valueBoxOutput('vbx_ast.turnover')
                                ),
                                column(width = 12,
                                       valueBoxOutput('vbx_ocf.NI'))
                              )      
                     )
              )
      ),
      
      # Define the UI elements using the variables
      tabItem(tabName = "calculator",
              tabBox(title = "Calculator", width = "auto",
                     tabPanel("Warnings",
                              fluidRow(
                                column(width = 12,
                                       textOutput("nofreecashflow"),
                                       #valueBoxOutput("vbx_fcf") 
                                ),
                                column(width = 12,
                                       textOutput("nooperatingcashflow"),
                                       #valueBoxOutput('vbx_OPCF.growth')
                                ),
                                column(width = 12,
                                       textOutput("notdoingbusiness"),
                                       #valueBoxOutput('vbx_rev.growth'),
                                       #valueBoxOutput('vbx_OPCF.growth')
                                ),
                                column(width = 12,
                                       textOutput("notgettingcashback"),
                                       #valueBoxOutput('vbx_opcf.NI')
                                )
                              )
                     )
              )
      ),
      
      tabItem(tabName = "about",
              tags$head(
                tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
              
              fluidRow(
                column(width = 12, h2("About The YNow App")),
                column(width = 8,
                       p("Red flags that may indicate financial fraud:", br(),
                         "- Unusual or unexpected increases in revenue or profits", br(),
                         "- Large, round numbers in the financial reports", br(),
                         "- Inflated or overstated assets", br(),
                         "- Unusual or unnecessary expenses or transfers between accounts", br(),
                         "- Unusual or inconsistent ratios or trends in the financial statements", br(),
                         "- Lack of adequate documentation or supporting evidence for transactions", br(),
                         "- Conflicts of interest among management or employees"
                       )
                ),
                column(width = 4,
                       h5("This is a web-based framework for building interactive data dashboards in R. 
                       The script loads a set of R packages for data processing, visualization and presentation, 
                       and sets up some functions for retrieving financial data from Yahoo Finance, 
                       extracting the relevant period data from the financial reports, 
                       and presenting the information in the form of tables and plots. 
                       The user interface is defined in the ui object and includes various inputs such as a text input for entering a stock code, 
                       info boxes for presenting summary information, and tabbed panels for displaying the finance summary, income statement and balance sheet. 
                       The data retrieval and processing are performed in the get.data and f4Periods functions, 
                       and the presentation of the data is done using Shiny components such as dataTableOutput and downloadButton."
                       )
                )
              )
      )
    )
  )
)
