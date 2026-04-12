# ui.R

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "The YNow App",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    collapsed = TRUE,
    column(
      width = 12,
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
      hr()
    ),
    column(
      width = 12,
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Calculator", tabName = "calculator", icon = icon("equalizer", lib = "glyphicon"), badgeLabel = "new", badgeColor = "green"),
        menuItem("About", tabName = "about", icon = icon("info-sign", lib = "glyphicon"))
      ),
      hr()
    ),
    column(
      width = 12,
      h5("Recent Search:"),
      textOutput("recentsearch"),
      hr()
    ),
    column(width = 12, textOutput("today"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('.main-header .logo { font-weight: bold }'))),
    
    fluidRow(
      column(width = 12, titlePanel(h2("Let's find some frauds from financial reports!"))),
      column(
        width = 8,
        titlePanel(h5("a lawrence kuo shiny app")),
        textInput("sc", "Stock Code", value = "AAPL"),
        actionButton("search", "Search", icon = icon("search"))
      ),
      column(width = 4, textOutput("txt_corpname"))
    ),
    br(),
    
    fluidRow(
      infoBoxOutput("ibx_marketcap"),
      infoBoxOutput("ibx_stockprice"),
      infoBoxOutput("ibox_EPS")
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        tabBox(
          title = "Financial Reports",
          width = "auto",
          
          tabPanel(
            "Finance Summary",
            p("This section imports Finance Summaries from Yahoo Finance"),
            dataTableOutput("tbFinanceSummary"),
            downloadButton('FS_download', "Download the data")
          ),
          
          tabPanel(
            "Income Statement",
            p("This section imports Income Statements from Yahoo Finance"),
            fluidRow(
              column(
                width = 6,
                selectizeInput("is_breakdown", h5("Breakdown Options"), choices = vISbreakdowns, selected = "Total Revenue", multiple = FALSE),
                plotlyOutput("plt_revenue", height = 300)
              )
            ),
            dataTableOutput("tbIncomeStatement"),
            downloadButton('IS_download', "Download the data")
          ),
          
          tabPanel(
            "Balance Sheet",
            p("This section imports Balance Sheets from Yahoo Finance"),
            fluidRow(
              column(
                width = 6,
                selectizeInput("bs_breakdown", h5("Breakdown Options"), choices = vBSbreakdowns, selected = c("Total Assets", "Total Debt"), multiple = TRUE),
                plotOutput("pieCapitalStructure2", height = 300)
              ),
              column(width = 6, plotOutput("pieCapitalStructure", height = 300))
            ),
            dataTableOutput("tbBalanceSheet"),
            downloadButton('BS_download', "Download the data")
          ),
          
          tabPanel(
            "Cash Flow",
            p("This section imports Cash Flow data from Yahoo Finance"),
            plotOutput("plt_3cashflow"),
            dataTableOutput("tbCashFlow"),
            downloadButton('CF_download', "Download the data")
          )
        ),
        
        tabBox(
          title = "Performance",
          width = "auto",
          
          tabPanel(
            "KPI by Sheet",
            fluidRow(
              column(width = 12, h4("Balance Sheet KPI"), valueBoxOutput('vbx_eqt_multiplier')),
              column(
                width = 12,
                h4("Income Statement KPI"),
                valueBoxOutput('vbx_rev_growth'),
                valueBoxOutput('vbx_gross_profit_growth'),
                valueBoxOutput('vbx_net_income_EBIT'),
                valueBoxOutput('vbx_gross_profit_margin'),
                valueBoxOutput('vbx_net_profit_margin')
              ),
              column(
                width = 12,
                h4("Cash Flow Statement KPI"),
                valueBoxOutput('vbx_op_cash_flow_growth'),
                valueBoxOutput('vbx_inv_cash_flow_growth'),
                valueBoxOutput('vbx_fin_cash_flow_growth')
              )
            )
          ),
          
          tabPanel(
            "Crossover KPI",
            fluidRow(
              column(
                width = 12,
                valueBoxOutput('vbx_ROA'),
                valueBoxOutput('vbx_ROE'),
                valueBoxOutput('vbx_asset_turnover')
              ),
              column(width = 12, valueBoxOutput('vbx_ocf_net_income'))
            )
          )
        )
      ),
      
      tabItem(
        tabName = "calculator",
        tabBox(
          title = "Calculator",
          width = "auto",
          
          tabPanel(
            "Warnings",
            fluidRow(
              column(width = 12, textOutput("nofreecashflow")),
              column(width = 12, textOutput("nooperatingcashflow")),
              column(width = 12, textOutput("notdoingbusiness")),
              column(width = 12, textOutput("notgettingcashback"))
            )
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
        fluidRow(
          column(width = 12, h2("About The YNow App")),
          column(
            width = 8,
            p(
              "Red flags that may indicate financial fraud:", br(),
              "- Unusual or unexpected increases in revenue or profits", br(),
              "- Large, round numbers in the financial reports", br(),
              "- Inflated or overstated assets", br(),
              "- Unusual or unnecessary expenses or transfers between accounts", br(),
              "- Unusual or inconsistent ratios or trends in the financial statements", br(),
              "- Lack of adequate documentation or supporting evidence for transactions", br(),
              "- Conflicts of interest among management or employees"
            )
          ),
          column(
            width = 4,
            h5("This is a web-based framework for building interactive data dashboards in R. The script loads a set of R packages for data processing, visualization, and presentation, and sets up some functions for retrieving financial data from Yahoo Finance, extracting the relevant period data from the financial reports, and presenting the information in the form of tables and plots. The user interface is defined in the ui object and includes various inputs such as a text input for entering a stock code, info boxes for presenting summary information, and tabbed panels for displaying the finance summary, income statement, and balance sheet. The data retrieval and processing are performed in the get.data and f4Periods functions, and the presentation of the data is done using Shiny components such as dataTableOutput and downloadButton.")
          )
        )
      )
    )
  )
)
