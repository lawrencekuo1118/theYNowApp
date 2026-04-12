# 清除全域環境中的所有對象
rm(list = ls(all = TRUE))

# 設定 knitr
knitr::opts_chunk$set(comment = NA)
knitr::opts_knit$set(global.par = TRUE)

# 設定全域選項
options(scipen = 20, digits = 4, width = 90)

# 安裝並加載所需的R包
if (!require(devtools)) install.packages("devtools")
if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  dplyr, ggplot2, ggrepel, Hmisc, jsonlite, 
  lubridate, magrittr, NLP, plotly, readr, 
  readxl, rio, reshape2, rvest, stats,
  stringr, xml2, tidyverse, shiny, shinydashboard, 
  DT, shinyjs, wordcloud, tm, jiebaR, openxlsx, 
  data.table, DBI, RMySQL
)

# 確保後續使用到的庫已加載
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

source("helpers.R")