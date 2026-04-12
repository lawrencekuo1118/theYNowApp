source("industry_standards.R")

# 自訂函數：轉換數字為 K / M / B 格式
format_dollar_abbr <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return("N/A")
  
  if (abs(x) >= 1e9) {
    paste0("$", round(x / 1e9, 2), "B")
  } else if (abs(x) >= 1e6) {
    paste0("$", round(x / 1e6, 2), "M")
  } else {
    paste0("$", round(x, 2))
  }
}

create_info_box <- function(title, value, icon_name, color = "blue", fill = TRUE) {
  infoBox(
    title = title,
    value = if (is.null(value) || is.na(value)) "N/A" else value,
    icon = icon(icon_name),
    color = color,
    fill = fill
  )
}

get_box_color <- function(industry, metric, value) {
  # 若 value 是 NA，顯示灰色
  if (is.na(value)) return("white")
  
  # 若為負數，一律紅字
  if (value < 0) return("red")
  
  # 若沒有該產業或指標的標準區間，顯示黑色
  if (!industry %in% names(industry_standards) || 
      is.null(industry_standards[[industry]][[metric]]) || 
      length(industry_standards[[industry]][[metric]]) != 2) {
    return("black")
  }
  
  bounds <- industry_standards[[industry]][[metric]]
  
  if (value < bounds[1]) {
    return("navy")  # too cold/low
  } else if (value > bounds[2]) {
    return("red")   # too hot/high
  } else {
    return("black") # in range
  }
}
