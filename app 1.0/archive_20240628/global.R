# 在開始任何事情之前，清除全域環境中的所有對象
rm(list = ls(all = TRUE))

# 設定 knitr
knitr::opts_chunk$set(comment = NA)
knitr::opts_knit$set(global.par = TRUE)

# 設定全域選項
options(scipen = 20, digits = 4, width = 90)

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
library(DT); library(shinycustomloader); library(shinyjs)

library(shiny)
library(shinydashboard)
library(rvest)
library(dplyr)
library(wordcloud)
library(tm)
library(jiebaR)
library(openxlsx)
library(data.table)
library(DBI)
library(RMySQL)