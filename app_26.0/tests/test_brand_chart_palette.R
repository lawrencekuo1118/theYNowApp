# test_brand_chart_palette.R — logo brand → BBLab plotly series palette
# Offline: pure-R expand / fallback / contrast; wiring smoke for BBLab module.
# Run: cd app_26.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_brand_chart_palette.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- normalizePath("..", mustWork = TRUE)
setwd(app_dir)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("PASS:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
  }
}

# Mirror pure helpers from web_crawler.R (avoid sourcing reticulate / caches).
`%||%` <- function(a, b) if (is.null(a)) b else a
.normalize_logo_hex <- function(cols) {
  cols <- trimws(as.character(unlist(cols, use.names = FALSE)))
  cols <- cols[!is.na(cols) & nzchar(cols)]
  cols <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
  unique(toupper(head(cols, 2L)))
}
.is_near_black_hex <- function(hex) {
  hex <- toupper(trimws(as.character(hex %||% "")[1]))
  if (!grepl("^#[0-9A-F]{6}$", hex)) return(FALSE)
  r <- strtoi(substr(hex, 2L, 3L), 16L)
  g <- strtoi(substr(hex, 4L, 5L), 16L)
  b <- strtoi(substr(hex, 6L, 7L), 16L)
  isTRUE(max(r, g, b, na.rm = TRUE) <= 40L)
}
.is_near_white_hex <- function(hex) {
  hex <- toupper(trimws(as.character(hex %||% "")[1]))
  if (!grepl("^#[0-9A-F]{6}$", hex)) return(FALSE)
  r <- strtoi(substr(hex, 2L, 3L), 16L)
  g <- strtoi(substr(hex, 4L, 5L), 16L)
  b <- strtoi(substr(hex, 6L, 7L), 16L)
  isTRUE(min(r, g, b, na.rm = TRUE) >= 235L)
}
.YNOW_DEFAULT_CHART_PALETTE <- c(
  "#0C5484", "#1AA8B8", "#249C60", "#E8A838", "#8E6BB5", "#D96B5F"
)
.hex_to_rgb01 <- function(hex) {
  hex <- toupper(trimws(as.character(hex %||% "")[1]))
  if (!grepl("^#[0-9A-F]{6}$", hex)) return(c(0, 0, 0))
  c(
    strtoi(substr(hex, 2L, 3L), 16L) / 255,
    strtoi(substr(hex, 4L, 5L), 16L) / 255,
    strtoi(substr(hex, 6L, 7L), 16L) / 255
  )
}
.rgb01_to_hex <- function(rgb) {
  rgb <- as.numeric(rgb)
  if (length(rgb) < 3L || any(!is.finite(rgb))) return("#0C5484")
  r <- max(0, min(255, round(rgb[[1]] * 255)))
  g <- max(0, min(255, round(rgb[[2]] * 255)))
  b <- max(0, min(255, round(rgb[[3]] * 255)))
  sprintf("#%02X%02X%02X", r, g, b)
}
.rgb01_to_hsl <- function(rgb) {
  r <- rgb[[1]]; g <- rgb[[2]]; b <- rgb[[3]]
  mx <- max(r, g, b); mn <- min(r, g, b)
  l <- (mx + mn) / 2
  if (mx <= mn + 1e-9) return(c(0, 0, l))
  d <- mx - mn
  s <- if (l > 0.5) d / (2 - mx - mn) else d / (mx + mn)
  h <- if (mx == r) {
    ((g - b) / d) %% 6
  } else if (mx == g) {
    (b - r) / d + 2
  } else {
    (r - g) / d + 4
  }
  c((h * 60) %% 360, s, l)
}
.hsl_to_rgb01 <- function(h, s, l) {
  h <- ((as.numeric(h) %% 360) + 360) %% 360
  s <- max(0, min(1, as.numeric(s)))
  l <- max(0, min(1, as.numeric(l)))
  if (s <= 1e-9) return(c(l, l, l))
  q <- if (l < 0.5) l * (1 + s) else l + s - l * s
  p <- 2 * l - q
  hk <- h / 360
  t <- function(tc) {
    if (tc < 0) tc <- tc + 1
    if (tc > 1) tc <- tc - 1
    if (tc < 1 / 6) return(p + (q - p) * 6 * tc)
    if (tc < 1 / 2) return(q)
    if (tc < 2 / 3) return(p + (q - p) * (2 / 3 - tc) * 6)
    p
  }
  c(t(hk + 1 / 3), t(hk), t(hk - 1 / 3))
}
.rel_luminance_hex <- function(hex) {
  rgb <- .hex_to_rgb01(hex)
  chan <- function(c) {
    if (c <= 0.03928) c / 12.92 else ((c + 0.055) / 1.055)^2.4
  }
  0.2126 * chan(rgb[[1]]) + 0.7152 * chan(rgb[[2]]) + 0.0722 * chan(rgb[[3]])
}
.chart_color_contrast_safe <- function(hex) {
  hex <- toupper(trimws(as.character(hex %||% "")[1]))
  if (!grepl("^#[0-9A-F]{6}$", hex)) return("#0C5484")
  L <- .rel_luminance_hex(hex)
  if (isTRUE(L > 0.72)) {
    hsl <- .rgb01_to_hsl(.hex_to_rgb01(hex))
    return(.rgb01_to_hex(.hsl_to_rgb01(hsl[[1]], min(1, hsl[[2]] + 0.05), 0.42)))
  }
  if (isTRUE(L < 0.04)) {
    return("#2A2A2A")
  }
  hex
}
brand_chart_palette <- function(brand_cols = NULL, n = 6L) {
  n <- suppressWarnings(as.integer(n)[1])
  if (!is.finite(n) || n < 1L) n <- 6L
  default <- .YNOW_DEFAULT_CHART_PALETTE
  cols <- .normalize_logo_hex(brand_cols)
  usable <- cols[!vapply(cols, .is_near_white_hex, logical(1))]
  if (!length(usable)) {
    return(toupper(rep_len(default, n)[seq_len(n)]))
  }
  seeds <- unique(vapply(usable, .chart_color_contrast_safe, character(1)))
  out <- character(0)
  for (s in seeds) {
    if (!s %in% out) out <- c(out, s)
  }
  primary <- seeds[[1]]
  hsl0 <- .rgb01_to_hsl(.hex_to_rgb01(primary))
  step <- 0L
  while (length(out) < n) {
    step <- step + 1L
    hue <- (hsl0[[1]] + step * 38) %% 360
    sat <- max(0.28, min(0.92, hsl0[[2]] * (if (step %% 2L) 0.92 else 1.05)))
    lit <- max(0.22, min(0.58, hsl0[[3]] + ((step %% 3L) - 1) * 0.08))
    cand <- .chart_color_contrast_safe(.rgb01_to_hex(.hsl_to_rgb01(hue, sat, lit)))
    if (!cand %in% out) out <- c(out, cand)
    if (step > n * 8L) {
      need <- n - length(out)
      out <- c(out, toupper(rep_len(default, need)))
      break
    }
  }
  toupper(out[seq_len(n)])
}

# ---- Unit asserts ----
fb <- brand_chart_palette(NULL, n = 6L)
check("empty brand → default length 6", identical(length(fb), 6L))
check("empty brand → default primary", identical(fb[[1]], "#0C5484"))
check("empty brand → default secondary", identical(fb[[2]], "#1AA8B8"))

fb3 <- brand_chart_palette(character(0), n = 3L)
check("empty brand n=3 truncates default", identical(fb3, toupper(.YNOW_DEFAULT_CHART_PALETTE[1:3])))

white_only <- brand_chart_palette(c("#FFFFFF", "#FEFEFE"), n = 4L)
check("white-only brand falls back", identical(white_only[[1]], "#0C5484"))

tsmc <- brand_chart_palette(c("#E60A1E", "#000000"), n = 6L)
check("brand primary is first slot", identical(tsmc[[1]], "#E60A1E"))
check("brand secondary kept when not white", identical(tsmc[[2]], "#000000") || identical(tsmc[[2]], "#2A2A2A"))
check("brand expands to n", identical(length(tsmc), 6L))
check("brand slots are distinct", identical(length(unique(tsmc)), 6L))
check("all slots are hex", all(grepl("^#[0-9A-F]{6}$", tsmc)))

pastel <- brand_chart_palette("#F8E8F0", n = 3L)
check("pastel primary contrast-adjusted", {
  L <- .rel_luminance_hex(pastel[[1]])
  is.finite(L) && L <= 0.72
})

near_black <- brand_chart_palette("#050505", n = 2L)
check("near-black lifted for visibility", {
  identical(near_black[[1]], "#2A2A2A") || .rel_luminance_hex(near_black[[1]]) >= 0.04
})

# Module wrappers (source without Shiny server)
source("business_breakdown_module.R", local = TRUE, encoding = "UTF-8")
check("module .bblab_chart_palette empty → default", {
  p <- .bblab_chart_palette(NULL, n = 6L)
  identical(p[[1]], "#0C5484") && length(p) == 6L
})
check("module .bblab_chart_palette uses brand", {
  p <- .bblab_chart_palette(c("#00A0E6", "#FAB400"), n = 4L)
  identical(p[[1]], "#00A0E6") && identical(p[[2]], "#FAB400")
})

# Wiring smoke
mod <- paste(readLines(file.path(app_dir, "business_breakdown_module.R"), warn = FALSE), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE), collapse = "\n")
wc <- paste(readLines(file.path(app_dir, "web_crawler.R"), warn = FALSE), collapse = "\n")
check("module defines .bblab_chart_palette", grepl(".bblab_chart_palette", mod, fixed = TRUE))
check("module accepts logo_colors_rv", grepl("logo_colors_rv = NULL", mod, fixed = TRUE))
check("module reads brand cols in charts", grepl(".brand_cols_now", mod, fixed = TRUE))
check("donut uses brand palette helper", grepl(".bblab_chart_palette(.brand_cols_now()", mod, fixed = TRUE))
check("hardcoded YNow pal removed from orders bar", {
  !grepl(
    'pal <- c("#0C5484", "#1AA8B8", "#249C60", "#E8A838", "#8E6BB5", "#D96B5F")',
    mod,
    fixed = TRUE
  )
})
check("server passes corp_name_logo_colors", {
  grepl("logo_colors_rv = corp_name_logo_colors", srv, fixed = TRUE)
})
check("crawler exports brand_chart_palette", grepl("brand_chart_palette <- function", wc, fixed = TRUE))
check("crawler keeps slow cache for logo colors", {
  grepl("cached_get_corp_logo_colors <- memoise::memoise", wc, fixed = TRUE) &&
    grepl("cache = .ynow_slow_cache", wc, fixed = TRUE)
})

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
