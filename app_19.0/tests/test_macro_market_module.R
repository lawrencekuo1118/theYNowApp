# Macro & Market Trends — catalog / isolation checks (no network)
# Run: cd app_19.0 && Rscript tests/test_macro_market_module.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
root <- if (file.exists("macro_market_module.R")) {
  normalizePath(".")
} else if (file.exists("../macro_market_module.R")) {
  normalizePath("..")
} else if (dir.exists("app_19.0") && file.exists("app_19.0/macro_market_module.R")) {
  normalizePath("app_19.0")
} else if (dir.exists("app_18.0") && file.exists("app_18.0/macro_market_module.R")) {
  normalizePath("app_18.0")
} else {
  stop("Cannot locate macro_market_module.R")
}
setwd(root)

source("debug_helpers.R", local = TRUE, encoding = "UTF-8")
source("market_profile.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("lab_concept_groups.R", local = TRUE, encoding = "UTF-8")
source("industry_standards.R", local = TRUE, encoding = "UTF-8")
source("lab_tw_universe.R", local = TRUE, encoding = "UTF-8")
fetch_price_history_df <- function(...) NULL
estimate_rolling_beta <- function(...) NA_real_
source("macro_market_module.R", local = TRUE, encoding = "UTF-8")

check <- function(label, ok) {
  if (!isTRUE(ok)) stop("FAIL: ", label)
  cat("OK ", label, "\n", sep = "")
}

check("US bench", identical(macro_bench_ticker("US"), "^GSPC"))
check("TW bench", identical(macro_bench_ticker("TW"), "^TWII"))

click_us <- macro_click_index_specs("US")
click_tw <- macro_click_index_specs("TW")
check("click specs follow market", identical(click_us, macro_index_specs("US")))
check("TW click specs follow TW catalog", identical(click_tw, macro_index_specs("TW")))
check("US and TW click catalogs differ", !identical(click_us, click_tw))
check(
  "US click specs four Yahoo symbols",
  identical(names(click_us), c("^GSPC", "^IXIC", "^DJI", "^SOX"))
)
check(
  "US click specs four labels",
  identical(unname(click_us), c("S&P 500", "Nasdaq", "Dow Jones", "SOX (semis)"))
)
check(
  "TW click specs three Yahoo symbols",
  identical(names(click_tw), c("^TWII", "IX0043.TWO", "0050.TW"))
)
check("TW catalog length 3", length(click_tw) == 3L)
check(
  "TW click specs three labels",
  identical(unname(click_tw), c("TAIEX", "TPEx", "0050"))
)
check("TW catalog has no industry sub-indices", !any(names(click_tw) %in% c("^TELI", "^TFNI")))
check("TW catalog has no US boards", !any(names(click_tw) %in% c("^GSPC", "^IXIC", "^DJI", "^SOX")))
check("US catalog has no TW boards", !any(names(click_us) %in% c("^TWII", "IX0043.TWO", "0050.TW", "^TELI", "^TFNI")))
check("US catalog still four", length(click_us) == 4L)
check("box id GSPC", identical(macro_index_box_id("^GSPC"), "ynow_macro_idx_gspc"))
check("box id IXIC", identical(macro_index_box_id("^IXIC"), "ynow_macro_idx_ixic"))
check("box id DJI", identical(macro_index_box_id("^DJI"), "ynow_macro_idx_dji"))
check("box id SOX", identical(macro_index_box_id("^SOX"), "ynow_macro_idx_sox"))
check("box id TWII", identical(macro_index_box_id("^TWII"), "ynow_macro_idx_twii"))
check("box id TWOII", identical(macro_index_box_id("IX0043.TWO"), "ynow_macro_idx_twoii"))
check("box id 0050", identical(macro_index_box_id("0050.TW"), "ynow_macro_idx_0050"))
check("name key GSPC", identical(macro_index_name_key("^GSPC"), "macro_index_name_gspc"))
check("name key TWII", identical(macro_index_name_key("^TWII"), "macro_index_name_twii"))
check("name key TWOII", identical(macro_index_name_key("IX0043.TWO"), "macro_index_name_twoii"))
check("name key 0050", identical(macro_index_name_key("0050.TW"), "macro_index_name_0050"))
for (sym in names(click_tw)) {
  check(
    paste("TW click maps", sym),
    identical(as.character(sym), names(click_tw)[match(sym, names(click_tw))]) &&
      grepl("^ynow_macro_idx_", macro_index_box_id(sym))
  )
}

ch_us <- macro_theme_choices("US", "en")
ch_tw <- macro_theme_choices("TW", "zh-TW")
ind_us <- macro_industry_choices("US", "en")
ind_tw <- macro_industry_choices("TW", "zh-TW")
con_us <- macro_concept_choices("US", "en")
con_tw <- macro_concept_choices("TW", "zh-TW")
check("US themes nonempty", length(ch_us) > 10)
check("TW themes nonempty", length(ch_tw) > 5)
check("US industry is GICS only", length(ind_us) >= 11L && all(startsWith(unname(ind_us), "gics_")))
check("US industry has no concept keys", !any(startsWith(unname(ind_us), "concept_")))
snap_zh <- industry_picker_choices("zh-TW")
check("TW industry matches snapshot", identical(ind_tw, snap_zh) && length(ind_tw) > 20L)
check("TW industry en matches snapshot", identical(macro_industry_choices("TW", "en"), industry_picker_choices("en")))
foundry_tks <- macro_theme_tickers("sc.Foundry", "TW")
check("TW foundry basket includes 2330", "2330.TW" %in% foundry_tks && length(foundry_tks) > 10L)
check("TW snapshot key is not a concept row", !"sc.Foundry" %in% unname(con_tw))
check("TW industry default is snapshot foundry", identical(macro_industry_default_key("TW"), "sc.Foundry"))
check("US industry default key", identical(macro_industry_default_key("US"), "gics_xlk"))
check("US concept has mag7", "concept_mag7" %in% unname(con_us))
check("US concept has no GICS keys", !any(startsWith(unname(con_us), "gics_")))
check("TW concept has shipping", "tw_shipping" %in% unname(con_tw))
check("US has XLK gics", "gics_xlk" %in% unname(ch_us) && "gics_xlk" %in% unname(ind_us))
check("XLK ticker", identical(macro_theme_tickers("gics_xlk", "US"), "XLK"))
check("TW shipping has 2603", "2603.TW" %in% macro_theme_tickers("tw_shipping", "TW"))
check("empty key no tickers", identical(macro_theme_tickers("", "US"), character(0)))
check("empty rebased is NULL", is.null(macro_rebased_theme_df("", "US", "1y")))
check("unknown rebased is NULL", is.null(macro_rebased_theme_df("gics_xlk", "US", "1y")))

none_en <- macro_none_choice("en")
none_zh <- macro_none_choice("zh-TW")
check("none en empty value", identical(unname(none_en), ""))
check("none zh empty value", identical(unname(none_zh), ""))
check("none en label", identical(names(none_en), ui_str("macro_none_option", "en")))
check("none zh label", identical(names(none_zh), ui_str("macro_none_option", "zh-TW")))
with_none <- macro_choices_with_none(ind_us, "en")
check("none prepended", identical(unname(with_none)[1], "") && "gics_xlk" %in% unname(with_none))

for (k in c(
  "menu_macro_market", "macro_fx_lock", "macro_page_title", "macro_theme_title",
  "macro_industry_label", "macro_concept_label", "macro_none_option",
  "macro_plot_need_pick", "macro_series_industry", "macro_series_concept",
  "macro_series_rebased", "macro_bubble_theme_label",
  "macro_index_name_gspc", "macro_index_name_ixic", "macro_index_name_dji",
  "macro_index_name_sox", "macro_index_name_twii", "macro_index_name_twoii",
  "macro_index_name_0050",
  "macro_index_chart_hint", "macro_index_chart_empty",
  "macro_index_chart_error",
  "hccsi_title", "hccsi_disclosure", "hccsi_index_health",
  "hccsi_index_stress", "hccsi_index_fragility", "hccsi_index_market",
  "hccsi_unavailable", "notif_hccsi_history_missing"
)) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
  check(paste("zh key present", k), !is.null(.UI_STRINGS$`zh-TW`[[k]]))
}
check("en industry label", identical(ui_str("macro_industry_label", "en"), "Industry vs benchmark"))
check("zh industry label", identical(ui_str("macro_industry_label", "zh-TW"), "產業別 vs 大盤"))
check("en concept label", identical(ui_str("macro_concept_label", "en"), "Concept vs benchmark"))
check("zh concept label", identical(ui_str("macro_concept_label", "zh-TW"), "概念股別 vs 大盤"))
check("en title not combined", !grepl("Industry / concept vs benchmark", ui_str("macro_theme_title", "en"), fixed = TRUE))
check("zh title not combined", !grepl("產業／概念股 vs 大盤", ui_str("macro_theme_title", "zh-TW"), fixed = TRUE))
check("zh none is 不選", identical(ui_str("macro_none_option", "zh-TW"), "不選"))
check("en index names stay English", identical(ui_str("macro_index_name_gspc", "en"), "S&P 500"))
check("zh index names stay English", identical(ui_str("macro_index_name_gspc", "zh-TW"), "S&P 500"))
check("en SOX name", identical(ui_str("macro_index_name_sox", "en"), "SOX (semis)"))
check("zh SOX name", identical(ui_str("macro_index_name_sox", "zh-TW"), "SOX (semis)"))
check("en TAIEX name", identical(ui_str("macro_index_name_twii", "en"), "TAIEX"))
check("zh TAIEX name", identical(ui_str("macro_index_name_twii", "zh-TW"), "加權（TAIEX）"))
check("en TPEx name", identical(ui_str("macro_index_name_twoii", "en"), "TPEx"))
check("zh TPEx name", identical(ui_str("macro_index_name_twoii", "zh-TW"), "櫃買（TPEx）"))
check("en 0050 name stays ticker", identical(ui_str("macro_index_name_0050", "en"), "0050"))
check("zh 0050 name", identical(ui_str("macro_index_name_0050", "zh-TW"), "0050（台灣50）"))
check("unused TELI locale dropped", is.null(.UI_STRINGS$en$macro_index_name_teli) &&
        is.null(.UI_STRINGS$`zh-TW`$macro_index_name_teli))
check("unused TFNI locale dropped", is.null(.UI_STRINGS$en$macro_index_name_tfni) &&
        is.null(.UI_STRINGS$`zh-TW`$macro_index_name_tfni))
check("zh TW labels no simplified", {
  tw_labs <- paste(
    ui_str("macro_index_name_twii", "zh-TW"),
    ui_str("macro_index_name_twoii", "zh-TW"),
    ui_str("macro_index_name_0050", "zh-TW"),
    collapse = " "
  )
  !grepl("加权|柜买|电子|金融保险类|默认|数据|台湾50", tw_labs)
})
check("en chart hint", grepl("Click an index box", ui_str("macro_index_chart_hint", "en"), fixed = TRUE))
check("zh chart hint", grepl("點選指數方塊", ui_str("macro_index_chart_hint", "zh-TW"), fixed = TRUE))

for (k in c(
  "macro_beta_title", "macro_beta_warn_title", "macro_beta_warn_body",
  "macro_beta_latest", "macro_beta_kpi_hint", "macro_beta_need_data",
  "macro_beta_chart_title", "macro_rf_note"
)) {
  check(paste("en orphan", k), is.null(.UI_STRINGS$en[[k]]))
  check(paste("zh orphan", k), is.null(.UI_STRINGS$`zh-TW`[[k]]))
}

txt <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
check("no CAPM writeback", !grepl("updateNumericInput", txt, fixed = TRUE))
check("warn copy present", grepl("CAPM", txt, fixed = TRUE))
check("theme rolling beta title gone", !grepl("Theme Rolling β vs benchmark", txt, fixed = TRUE))
check("ynow_macro_beta_title gone", !grepl("ynow_macro_beta_title", txt, fixed = TRUE))
check("beta_plot output gone", !grepl("beta_plot", txt, fixed = TRUE))
check("beta_kpi output gone", !grepl("beta_kpi", txt, fixed = TRUE))
check("rolling beta engine kept", grepl("macro_rolling_beta_path", txt, fixed = TRUE))
check("estimate_rolling_beta still used", grepl("estimate_rolling_beta", txt, fixed = TRUE))

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE), collapse = "\n")
check("sidebar first macro", grepl('tabName = "macro_market"', ui_src, fixed = TRUE))
# macro menuItem appears before Dashboard menuItem
pos_macro <- regexpr('tabName = "macro_market"', ui_src, fixed = TRUE)[1]
pos_dash <- regexpr('tabName = "dashboard"', ui_src, fixed = TRUE)[1]
check("macro before dashboard in UI", is.finite(pos_macro) && pos_macro > 0 && pos_macro < pos_dash)
check(
  "hide ticker chrome on macro",
  grepl("input.sidebar_tabs != 'about' && input.sidebar_tabs != 'macro_market'", ui_src, fixed = TRUE)
)
check("locale wiring dropped beta title", !grepl("ynow_macro_beta_title", ui_src, fixed = TRUE))
check("locale wiring dropped beta warn", !grepl("macro_beta_warn_title", ui_src, fixed = TRUE))
check("locale wires industry label", grepl("ynow_macro_industry_label", ui_src, fixed = TRUE))
check("locale wires concept label", grepl("ynow_macro_concept_label", ui_src, fixed = TRUE))
check("locale dropped combined theme label", !grepl("ynow_macro_theme_label", ui_src, fixed = TRUE))

.notes_calls <- function(src) {
  calls <- character(0)
  remaining <- src
  repeat {
    m <- regexpr("ynow_notes_block\\s*\\(", remaining)
    if (m < 1L) break
    start <- as.integer(m)
    depth <- 0L
    end <- nchar(remaining)
    for (i in seq.int(start, nchar(remaining))) {
      ch <- substr(remaining, i, i)
      if (identical(ch, "(")) depth <- depth + 1L
      if (identical(ch, ")")) {
        depth <- depth - 1L
        if (depth <= 0L) {
          end <- i
          break
        }
      }
    }
    calls <- c(calls, substr(remaining, start, end))
    remaining <- substr(remaining, end + 1L, nchar(remaining))
  }
  calls
}
ui_start <- regexpr("macro_market_ui <- function", txt, fixed = TRUE)[1]
ui_end <- regexpr("# ---- Server ----", txt, fixed = TRUE)[1]
ui_fn <- if (ui_start > 0 && ui_end > ui_start) substr(txt, ui_start, ui_end) else txt
theme_sec_start <- regexpr("ynow_macro_theme_title", ui_fn, fixed = TRUE)[1]
theme_sec_end <- regexpr("# Bubble & concentration", ui_fn, fixed = TRUE)[1]
theme_sec <- if (theme_sec_start > 0 && theme_sec_end > theme_sec_start) {
  substr(ui_fn, theme_sec_start, theme_sec_end)
} else {
  ui_fn
}
theme_notes <- .notes_calls(theme_sec)
check("theme help present", grepl("ynow_macro_theme_help", theme_sec, fixed = TRUE))
check("theme help is chapter chrome", grepl("ynow-macro-chapter__lead", theme_sec, fixed = TRUE))
check("theme help not in Notes", !any(grepl("ynow_macro_theme_help", theme_notes, fixed = TRUE)))
check("theme help first-paint GICS", grepl("GICS sector ETFs", theme_sec, fixed = TRUE))
check("theme help first-paint pick independently",
      grepl("Pick Industry and Concept independently", theme_sec, fixed = TRUE))
check("no empty top Notes before pickers", {
  pos_help <- regexpr("ynow_macro_theme_help", theme_sec, fixed = TRUE)[1]
  pos_pick <- regexpr("industry_key", theme_sec, fixed = TRUE)[1]
  if (!(pos_help > 0 && pos_pick > pos_help)) {
    FALSE
  } else {
    head <- substr(theme_sec, 1L, pos_pick)
    !grepl("ynow_notes_block", head, fixed = TRUE)
  }
})
check("bottom Notes is currency lock", {
  length(theme_notes) == 1L &&
    grepl("ynow_macro_fx_lock", theme_notes[[1]], fixed = TRUE) &&
    !grepl("ynow_macro_theme_help", theme_notes[[1]], fixed = TRUE)
})
check("bottom Notes after chart", {
  pos_plot <- regexpr("overlay_plot", theme_sec, fixed = TRUE)[1]
  pos_notes <- regexpr("ynow_notes_block", theme_sec, fixed = TRUE)[1]
  pos_fx <- regexpr("ynow_macro_fx_lock", theme_sec, fixed = TRUE)[1]
  is.finite(pos_plot) && pos_plot > 0 && pos_notes > pos_plot && pos_fx > pos_notes
})
check("overlay plot not inside leftover notes", !any(grepl("overlay_plot", theme_notes, fixed = TRUE)))
check("pickers not inside leftover notes", !any(grepl("industry_key", theme_notes, fixed = TRUE)))
check("theme help not lite-hidden", !grepl("ynow_macro_theme_help\"[^\n]*ynow-full-only", theme_sec))
check("en theme help GICS", grepl("GICS", ui_str("macro_theme_help", "en"), fixed = TRUE))
check("zh theme help GICS", grepl("GICS", ui_str("macro_theme_help", "zh-TW"), fixed = TRUE))
check("en theme help Industry", grepl("Pick Industry and Concept independently",
                                     ui_str("macro_theme_help", "en"), fixed = TRUE))
check("zh theme help 獨立選單", grepl("獨立選單", ui_str("macro_theme_help", "zh-TW"), fixed = TRUE))
check("zh theme help no simplified", !grepl("独立|菜单|数据", ui_str("macro_theme_help", "zh-TW")))
if (requireNamespace("htmltools", quietly = TRUE) && exists("macro_market_ui", mode = "function")) {
  ui_html <- tryCatch({
    paste(as.character(macro_market_ui()), collapse = "")
  }, error = function(e) "")
  if (nzchar(ui_html)) {
    help_pos <- regexpr("id=\"ynow_macro_theme_help\"", ui_html, fixed = TRUE)[1]
    details <- gregexpr("<details[^>]*class=\"[^\"]*ynow-notes", ui_html)[[1]]
    fx_pos <- regexpr("id=\"ynow_macro_fx_lock\"", ui_html, fixed = TRUE)[1]
    plot_pos <- regexpr("overlay_plot", ui_html, fixed = TRUE)[1]
    check("rendered theme help exists", help_pos > 0)
    check("rendered theme help outside details", {
      if (help_pos < 1L) {
        FALSE
      } else if (length(details) == 0L || details[1] < 0L) {
        TRUE
      } else {
        !any(details < help_pos & (details + 400L) > help_pos)
      }
    })
    check("rendered bottom Notes after plot", {
      fx_in_details <- grepl(
        "<details[^>]*class=\"[^\"]*ynow-notes[\\s\\S]*?ynow_macro_fx_lock",
        ui_html
      )
      plot_pos > 0 && fx_pos > plot_pos && fx_in_details
    })
  } else {
    check("macro_market_ui render skipped", TRUE)
  }
}
check("overlay industry input", grepl('ns("industry_key")', txt, fixed = TRUE))
check("overlay concept input", grepl('ns("concept_key")', txt, fixed = TRUE))
check("combined overlay theme_key gone", !grepl('ns("theme_key")', txt, fixed = TRUE))
check("combined title gone", !grepl("Industry / concept vs benchmark", txt, fixed = TRUE))
check("industry vs benchmark first paint", grepl("Industry vs benchmark", txt, fixed = TRUE))
check("concept vs benchmark first paint", grepl("Concept vs benchmark", txt, fixed = TRUE))
check("industry defaults to technology", grepl('selected = "gics_xlk"', txt, fixed = TRUE))
check("industry server default follows market", grepl("macro_industry_default_key(mode)", txt, fixed = TRUE))
check("no Lite-only combined menu", !grepl("ynow-lite-only", txt, fixed = TRUE))
check("click specs helper", grepl("macro_click_index_specs", txt, fixed = TRUE))
check("KPI uses click specs by market", grepl("macro_click_index_specs(.mode())", txt, fixed = TRUE))
check("click validates against market catalog", grepl("macro_click_index_specs(.mode())", txt, fixed = TRUE))
check("clickable box id GSPC", grepl("ynow_macro_idx_gspc", txt, fixed = TRUE))
check("clickable box id IXIC", grepl("ynow_macro_idx_ixic", txt, fixed = TRUE))
check("clickable box id DJI", grepl("ynow_macro_idx_dji", txt, fixed = TRUE))
check("clickable box id SOX", grepl("ynow_macro_idx_sox", txt, fixed = TRUE))
check("clickable box id TWII", grepl("ynow_macro_idx_twii", txt, fixed = TRUE))
check("clickable box id TWOII", grepl("ynow_macro_idx_twoii", txt, fixed = TRUE))
check("clickable box id 0050", grepl("ynow_macro_idx_0050", txt, fixed = TRUE))
check("no TELI box id", !grepl("ynow_macro_idx_teli", txt, fixed = TRUE))
check("no TFNI box id", !grepl("ynow_macro_idx_tfni", txt, fixed = TRUE))
check("TW catalog has no TELI symbol", !grepl("^TELI", txt, fixed = TRUE))
check("TW catalog has no TFNI symbol", !grepl("^TFNI", txt, fixed = TRUE))
check("TW catalog includes 0050 KPI", grepl('"0050.TW" = "0050"', txt, fixed = TRUE))
check("TW three-card width helper", grepl("idx_col_w <- if (identical(n_idx, 3L)) 4L else 3L", txt, fixed = TRUE))
check("US four-card index columns stay col-md-3", grepl("col-xs-6 col-sm-6 col-md-3", txt, fixed = TRUE))
check("TW three-card index columns are col-md-4", grepl("col-xs-12 col-sm-4 col-md-4", txt, fixed = TRUE))
check("index click input", grepl("index_click", txt, fixed = TRUE))
check("index hist plot output", grepl("index_hist_plot", txt, fixed = TRUE))
check("index hist panel above Rf", {
  pos_hist <- regexpr("ynow_macro_index_hist", txt, fixed = TRUE)[1]
  pos_rf <- regexpr("rf_signal_row", txt, fixed = TRUE)[1]
  is.finite(pos_hist) && pos_hist > 0 && is.finite(pos_rf) && pos_hist < pos_rf
})
check("hint above Rf", {
  pos_hint <- regexpr("ynow_macro_index_hint", txt, fixed = TRUE)[1]
  pos_rf <- regexpr("rf_signal_row", txt, fixed = TRUE)[1]
  is.finite(pos_hint) && pos_hint > 0 && pos_hint < pos_rf
})
check("chart slot is full-only", grepl("ynow-macro-index-hist ynow-full-only", txt, fixed = TRUE))
check("hint is full-only", grepl("ynow-macro-hint ynow-full-only", txt, fixed = TRUE))
check("lite gate on click", grepl(".is_lite", txt, fixed = TRUE) && grepl("lite_mode_rv", txt, fixed = TRUE))
check("click handler does not blank selection", {
  start <- regexpr("observeEvent(input$index_click", txt, fixed = TRUE)[1]
  rest <- if (start > 0) substr(txt, start, start + 420L) else ""
  grepl("macro_click_index_specs(.mode())", rest, fixed = TRUE) &&
    !grepl("selected_index(\"\")", rest, fixed = TRUE)
})
check("market switch clears foreign board", grepl("selected_index(\"\")", txt, fixed = TRUE))
check("no Theme Rolling beta return", !grepl("Theme Rolling β vs benchmark", txt, fixed = TRUE))
check("Rf CAPM note sentence gone", !grepl("Same live Rf path as CAPM", txt, fixed = TRUE))
check("Rf note id gone", !grepl("ynow_macro_rf_note", txt, fixed = TRUE))
check("Rf KPI kept", grepl("ynow-macro-kpi--rf", txt, fixed = TRUE))
check("Rf box kept", grepl("rf_box", txt, fixed = TRUE))
check("live CAPM Rf path kept", grepl("cached_get_risk_free_rate", txt, fixed = TRUE))
check("Rf row has no Notes wrapper", {
  pos <- regexpr("output$rf_signal_row", txt, fixed = TRUE)[1]
  pos2 <- regexpr("overlay_data <- reactive", txt, fixed = TRUE)[1]
  chunk <- if (pos > 0 && pos2 > pos) substr(txt, pos, pos2) else ""
  nzchar(chunk) && !grepl("ynow_notes_block", chunk, fixed = TRUE) &&
    grepl("rf_box", chunk, fixed = TRUE) && grepl("macro_rf_title", chunk, fixed = TRUE)
})
check("locale dropped rf note wiring", !grepl("ynow_macro_rf_note", ui_src, fixed = TRUE))
check("locale dropped rf note sentence", !grepl("Same live Rf path as CAPM", ui_src, fixed = TRUE))

ui_css <- ui_src
check("lite CSS hides chart slot", grepl("body.ynow-lite #ynow_macro_index_hist", ui_css, fixed = TRUE))
check("lite CSS disables clickable boxes", grepl("body.ynow-lite .ynow-macro-kpi--clickable", ui_css, fixed = TRUE))
check("TW market red-up green-down CSS", {
  grepl("body.ynow-market-tw .ynow-macro-up", ui_css, fixed = TRUE) &&
    grepl("body.ynow-market-tw .ynow-macro-down", ui_css, fixed = TRUE) &&
    grepl("body.ynow-market-tw .ynow-macro-kpi .ynow-macro-up", ui_css, fixed = TRUE)
})
check("locale wires index hint", grepl("ynow_macro_index_hint", ui_css, fixed = TRUE))
check("HCCSI box on Rf row", grepl("ynow-macro-kpi--hccsi", txt, fixed = TRUE))
check("YNOW KPI on Rf row", grepl("ynow-macro-kpi--ynow", txt, fixed = TRUE) &&
  grepl(".own_index_kpi_card", txt, fixed = TRUE))
check("HCCSI expand Full-only", grepl("ynow-macro-hccsi-expand ynow-full-only", txt, fixed = TRUE))
check("HCCSI expand below Rf row", {
  pos_rf <- regexpr("rf_signal_row", txt, fixed = TRUE)[1]
  pos_ex <- regexpr("ynow_macro_hccsi_expand", txt, fixed = TRUE)[1]
  is.finite(pos_rf) && is.finite(pos_ex) && pos_ex > pos_rf
})
check("RF:YNOW:HCCSI 2:1:1 widths", grepl("width = 6, class = \"col-xs-12 col-sm-6 col-md-6\"", txt, fixed = TRUE) &&
  grepl("width = 3, class = \"col-xs-12 col-sm-3 col-md-3\"", txt, fixed = TRUE) &&
  grepl("rf_col, ynow_col, hccsi_col", txt, fixed = TRUE))
check("TW NDC on next row", grepl("ynow-macro-ndc-row", txt, fixed = TRUE))
check("lite CSS hides HCCSI expand", grepl("body.ynow-lite #ynow_macro_hccsi_expand", ui_css, fixed = TRUE))
check("en HCCSI title", identical(ui_str("hccsi_title", "en"), "HCCSI"))
check("zh HCCSI title stays English", identical(ui_str("hccsi_title", "zh-TW"), "HCCSI"))

cat("All macro market module checks passed.\n")
