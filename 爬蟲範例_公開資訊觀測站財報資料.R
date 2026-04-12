# Clear environment
rm(list=ls())
gc()

library(RCurl)
library(dplyr)
library(XML)

# Set parameters
stockCode <- 2317   # Stock code for TSMC
reportYear <- 2017  # Reporting year
reportSeason <- 2   # Reporting season

# Construct URL
url <- paste0("http://mops.twse.com.tw/server-java/t164sb01?step=1&CO_ID=",
              stockCode,"&SYEAR=",reportYear,"&SSEASON=",reportSeason,"&REPORT_ID=C")

# Fetch the content only once
content <- getURL(url, .encoding="big5") %>%
  iconv(from="big5", to="utf-8") 

# Parse HTML content
parsed_content <- htmlParse(content, encoding="utf-8")

# Function to fetch table data with error handling
fetch_table_data <- function(parsed_content, xpath, col_num) {
  title <- xpathSApply(parsed_content, xpath, xmlValue)
  data <- xpathSApply(parsed_content, xpath, xmlValue)
  
  # Check if data is NULL or empty
  if (is.null(data) || length(data) == 0) {
    message("No data found for the given XPath: ", xpath)
    return(matrix(NA, ncol = col_num, nrow = 0))  # Return empty matrix
  }
  
  # Convert to matrix
  data_matrix <- matrix(data, ncol = col_num, byrow = TRUE)
  
  # Set column names
  colnames(data_matrix) <- title
  return(data_matrix)
}

# Fetch Balance Sheet
balanceSheet <- fetch_table_data(parsed_content, "//table[@class='result_table hasBorder']//tr[@class='tblHead'][1]/th", 4)

# Fetch Income Statement
incomeStatement <- fetch_table_data(parsed_content, "//table[@class='main_table hasBorder'][1]//tr[@class='tblHead'][1]/th", 5)

# Fetch Cash Flow Statement
cashFlow <- fetch_table_data(parsed_content, "//table[@class='main_table hasBorder'][2]//tr[@class='tblHead'][1]/th", 3)

# Print summaries of the data
if (!is.null(balanceSheet) && nrow(balanceSheet) > 0) {
  print("Balance Sheet:")
  print(head(balanceSheet))
}

if (!is.null(incomeStatement) && nrow(incomeStatement) > 0) {
  print("Income Statement:")
  print(head(incomeStatement))
}

if (!is.null(cashFlow) && nrow(cashFlow) > 0) {
  print("Cash Flow Statement:")
  print(head(cashFlow))
}
