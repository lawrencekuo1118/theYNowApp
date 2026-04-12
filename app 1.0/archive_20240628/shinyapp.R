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

############################## shinyapp
shinyApp(ui = ui, server = server)
