# Helper functions

get.data <- function(no.stock) {
  base_url <- "https://finance.yahoo.com/quote/"
  l_url <- list(
    paste0(base_url, no.stock),
    paste0(base_url, no.stock, "/?p"),
    paste0(base_url, no.stock, "/financials/?p=", no.stock),
    paste0(base_url, no.stock, "/balance-sheet/?p=", no.stock),
    paste0(base_url, no.stock, "/cash-flow/?p=", no.stock)
  )
  
  read_html_with_error_handling <- function(url) {
    tryCatch(
      {
        read_html(url)
      },
      error = function(e) {
        if (grepl("HTTP error 404", conditionMessage(e), fixed = TRUE)) {
          message("HTTP error 404 occurred for URL: ", url)
          return(NULL)
        } else {
          message("An error occurred for URL: ", url, "\n", conditionMessage(e))
          return(NULL)
        }
      }
    )
  }
  
  html_content_list <- lapply(l_url, read_html_with_error_handling)
  html_content_list <- Filter(Negate(is.null), html_content_list)
  
  return(html_content_list)
}

vISbreakdowns <- c("Total Revenue", "Cost of Revenue", "Gross Profit", 
                   "Operating Expense", "Operating Income", "Total Expenses", 
                   "Net Income from Continuing & Discontinued Operation", "EBIT", "EBITDA")

vBSbreakdowns <- c("Total Assets", "Total Liabilities Net Minority Interest", 
                   "Total Equity Gross Minority Interest", "Total Capitalization", 
                   "Common Stock Equity", "Capital Lease Obligations", "Net Tangible Assets", 
                   "Invested Capital", "Tangible Book Value", "Total Debt", "Net Debt", 
                   "Share Issued", "Ordinary Shares Number", "Treasury Shares Number")

fNum_tfrr <- function(df) {
  df_cleaned <- as.data.frame(lapply(df, function(x) {
    numeric_values <- as.numeric(gsub(",", "", x))
    ifelse(is.na(x), NA, numeric_values)
  }), stringsAsFactors = TRUE)
  return(df_cleaned)
}

Loader1 <- function(x) { withLoader(x, type = "html", loader = "loader1") }

BoxColor <- function(num) { 
  if (is.na(num)) return("black")
  if (as.numeric(num) < 0) return("red") else return("black")
}

