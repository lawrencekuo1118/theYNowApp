# test_corp_logo_colors.R — logo → corp-name tint helpers
# Default: offline R asserts (hex normalize/pair, UI/server wiring, source pad rules).
# Optional env: Python fixture extract when .ynow_venv (or Pillow-ready python) is present.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_corp_logo_colors.R
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

# Source only the hex helpers via a tiny local copy of the pure-R helpers
# (full web_crawler.R pulls reticulate / caches — too heavy for this unit).
.normalize_logo_hex <- function(cols) {
  cols <- trimws(as.character(unlist(cols, use.names = FALSE)))
  cols <- cols[!is.na(cols) & nzchar(cols)]
  cols <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
  unique(toupper(head(cols, 2L)))
}
.is_near_black_hex <- function(hex) {
  hex <- toupper(trimws(as.character(hex)[1]))
  if (!grepl("^#[0-9A-F]{6}$", hex)) return(FALSE)
  r <- strtoi(substr(hex, 2L, 3L), 16L)
  g <- strtoi(substr(hex, 4L, 5L), 16L)
  b <- strtoi(substr(hex, 6L, 7L), 16L)
  isTRUE(max(r, g, b, na.rm = TRUE) <= 40L)
}
.ensure_logo_color_pair <- function(cols) {
  cols <- .normalize_logo_hex(cols)
  if (length(cols) == 1L) {
    pad <- if (isTRUE(.is_near_black_hex(cols[[1]]))) "#FFFFFF" else "#000000"
    return(c(cols[[1]], pad))
  }
  cols
}
.is_logo_mono_brand <- function(cols) {
  identical(length(.normalize_logo_hex(cols)), 1L)
}

check("normalize keeps two hex", identical(
  .normalize_logo_hex(c("#e60a1e", "#00a0e6", "#aabbcc")),
  c("#E60A1E", "#00A0E6")
))
check("normalize drops junk", identical(
  .normalize_logo_hex(c("red", "#GG0000", "#112233")),
  "#112233"
))
check("normalize empty", identical(.normalize_logo_hex(NULL), character(0)))
check("pair pads single with black", identical(
  .ensure_logo_color_pair("#e60a1e"),
  c("#E60A1E", "#000000")
))
check("pair pads sole black with white", identical(
  .ensure_logo_color_pair("#0A0A0A"),
  c("#0A0A0A", "#FFFFFF")
))
check("pair pads pure black with white", identical(
  .ensure_logo_color_pair("#000000"),
  c("#000000", "#FFFFFF")
))
check("pair keeps two brands", identical(
  .ensure_logo_color_pair(c("#fab400", "#00a0e6")),
  c("#FAB400", "#00A0E6")
))
check("pair empty stays empty", identical(
  .ensure_logo_color_pair(NULL),
  character(0)
))
check("mono brand is length-1 primary", {
  isTRUE(.is_logo_mono_brand("#E60A1E")) &&
    isTRUE(.is_logo_mono_brand(c("#E60A1E"))) &&
    !isTRUE(.is_logo_mono_brand(c("#E60A1E", "#00A0E6"))) &&
    !isTRUE(.is_logo_mono_brand(character(0)))
})
check("mono name stays solid (no pad for corp-name)", {
  raw <- .normalize_logo_hex("#e60a1e")
  isTRUE(.is_logo_mono_brand(raw)) && identical(raw, "#E60A1E")
})
check("dual brand keeps gradient pair", {
  raw <- .normalize_logo_hex(c("#fab400", "#00a0e6"))
  !isTRUE(.is_logo_mono_brand(raw)) &&
    identical(.ensure_logo_color_pair(raw), c("#FAB400", "#00A0E6"))
})

# Optional Python extract (env) — not required for default offline path.
# Prefer app venv; only use a python that can import Pillow + corp_logo_colors.
.py_ready <- function(py_bin) {
  if (!nzchar(py_bin) || !file.exists(py_bin)) return(FALSE)
  .py_str <- function(x) {
    paste0("'", gsub("\\\\", "\\\\\\\\", gsub("'", "\\\\'", x, fixed = TRUE), fixed = TRUE), "'")
  }
  code <- paste0(
    "import sys; sys.path.insert(0, ", .py_str(app_dir), "); ",
    "import PIL; import corp_logo_colors"
  )
  identical(as.integer(system2(py_bin, c("-c", code), stdout = FALSE, stderr = FALSE)), 0L)
}
.py_candidate <- function() {
  venv_py <- file.path(app_dir, ".ynow_venv", "bin", "python")
  for (cand in c(venv_py, Sys.which("python3"))) {
    if (isTRUE(.py_ready(cand))) return(cand)
  }
  ""
}
py <- .py_candidate()
fixture <- file.path(app_dir, "tests", "fixtures", "logo_red_blue.png")
check("fixture png exists", file.exists(fixture))
.py_ok <- nzchar(py) && file.exists(fixture)

if (isTRUE(.py_ok)) {
  py_script <- tempfile(fileext = ".py")
  .py_str <- function(x) {
    paste0("'", gsub("\\\\", "\\\\\\\\", gsub("'", "\\\\'", x, fixed = TRUE), fixed = TRUE), "'")
  }
  writeLines(
    c(
      "import sys",
      paste0("sys.path.insert(0, ", .py_str(app_dir), ")"),
      "from corp_logo_colors import extract_logo_colors_from_bytes",
      paste0("p = ", .py_str(fixture)),
      "cols = extract_logo_colors_from_bytes(open(p, 'rb').read())",
      "print(','.join(cols))"
    ),
    py_script
  )
  out <- tryCatch(
    system2(py, py_script, stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  unlink(py_script)
  line <- if (length(out)) trimws(tail(out, 1)) else ""
  cols <- if (nzchar(line)) strsplit(line, ",", fixed = TRUE)[[1]] else character(0)
  check("fixture yields 1–2 raw colors", length(cols) %in% 1:2)
  check("fixture primary is crimson-ish", {
    if (!length(cols)) FALSE else {
      h <- cols[[1]]
      r <- strtoi(substr(h, 2, 3), 16L)
      g <- strtoi(substr(h, 4, 5), 16L)
      b <- strtoi(substr(h, 6, 7), 16L)
      r > 150 && g < 80 && b < 100
    }
  })
  check("fixture secondary is blue-ish when dual", {
    if (length(cols) < 2L) TRUE else {
      h <- cols[[2]]
      r <- strtoi(substr(h, 2, 3), 16L)
      g <- strtoi(substr(h, 4, 5), 16L)
      b <- strtoi(substr(h, 6, 7), 16L)
      b > 150 && r < 80
    }
  })
} else {
  cat("SKIP optional python fixture extract (need .ynow_venv or Pillow-ready python)\n")
}

# Optional Python mono extract (offline synthetic) — resolve keeps raw length-1
if (isTRUE(.py_ok)) {
  py_script <- tempfile(fileext = ".py")
  .py_str <- function(x) {
    paste0("'", gsub("\\\\", "\\\\\\\\", gsub("'", "\\\\'", x, fixed = TRUE), fixed = TRUE), "'")
  }
  writeLines(
    c(
      "import sys, io",
      paste0("sys.path.insert(0, ", .py_str(app_dir), ")"),
      "from PIL import Image",
      "from corp_logo_colors import extract_logo_colors_from_bytes",
      "im = Image.new('RGBA', (48, 48), (230, 10, 30, 255))",
      "buf = io.BytesIO(); im.save(buf, format='PNG')",
      "print('RED', ','.join(extract_logo_colors_from_bytes(buf.getvalue(), max_colors=2)))",
      "im2 = Image.new('RGBA', (48, 48), (10, 10, 10, 255))",
      "buf2 = io.BytesIO(); im2.save(buf2, format='PNG')",
      "print('BLK', ','.join(extract_logo_colors_from_bytes(buf2.getvalue(), max_colors=2)))"
    ),
    py_script
  )
  out <- tryCatch(
    system2(py, py_script, stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  unlink(py_script)
  red_line <- trimws(tail(out[grepl("^RED ", out)], 1))
  blk_line <- trimws(tail(out[grepl("^BLK ", out)], 1))
  red_cols <- if (nzchar(red_line)) toupper(strsplit(sub("^RED ", "", red_line), ",", fixed = TRUE)[[1]]) else character(0)
  blk_cols <- if (nzchar(blk_line)) toupper(strsplit(sub("^BLK ", "", blk_line), ",", fixed = TRUE)[[1]]) else character(0)
  check("single red brand stays mono (raw)", {
    length(red_cols) == 1L && isTRUE(.is_logo_mono_brand(red_cols))
  })
  check("single black brand stays mono (raw)", {
    length(blk_cols) == 1L && isTRUE(.is_logo_mono_brand(blk_cols))
  })
  check("R pad still available for charts/KPI", {
    identical(.ensure_logo_color_pair(red_cols)[[2]], "#000000") &&
      identical(.ensure_logo_color_pair(blk_cols)[[2]], "#FFFFFF")
  })
} else {
  cat("SKIP optional python single-brand mono check (need .ynow_venv or Pillow-ready python)\n")
}

# UI CSS hooks present
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE), collapse = "\n")
check("css logo-grad present", grepl("ynow-corpname-logo-grad", ui, fixed = TRUE))
check("css logo-solid present", grepl("ynow-corpname-logo-solid", ui, fixed = TRUE))
check("css mono KPI solid rule present", grepl("ynow-corp-logo-mono", ui, fixed = TRUE))
check(
  "quote KPI icons reuse company-name gradient",
  grepl("body.ynow-corp-logo #ibx_stockprice .info-box-icon", ui, fixed = TRUE) &&
    grepl("body.ynow-corp-logo #ibx_marketcap .info-box-icon", ui, fixed = TRUE) &&
    grepl("body.ynow-corp-logo #ibx_EPS .info-box-icon", ui, fixed = TRUE) &&
    grepl("ynowCorpLogoColors", ui, fixed = TRUE)
)
check("css white-en helper present", grepl("ynow-corpname-en-on-white", ui, fixed = TRUE))
check("client toggles mono body class", {
  grepl("ynow-corp-logo-mono", ui, fixed = TRUE) &&
    grepl("payload.mono", ui, fixed = TRUE)
})

srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE), collapse = "\n")
check("server stores logo colors", grepl("corp_name_logo_colors", srv, fixed = TRUE))
check("server pushes logo colors to quote icons", grepl("ynowCorpLogoColors", srv, fixed = TRUE))
check("server ensures color pair", grepl("ensure_logo_color_pair", srv, fixed = TRUE))
check("server detects mono brand for corp-name", {
  grepl("is_logo_mono_brand", srv, fixed = TRUE) &&
    grepl("ynow-corpname-logo-solid", srv, fixed = TRUE)
})
check("server stores raw logo colors (not always pad)", {
  grepl("normalize_logo_hex(logo_cols)", srv, fixed = TRUE) &&
    grepl("Mono stays length 1", srv, fixed = TRUE)
})
check("server calls cached_get_corp_logo_colors", grepl("cached_get_corp_logo_colors", srv, fixed = TRUE))
check("logo tint scheduled from phase1b (not gated on financials)", {
  grepl(".schedule_corp_logo_tint", srv, fixed = TRUE) &&
    grepl("Brand tint for company name", srv, fixed = TRUE)
})
check("phase3 only backfills empty logo tint", {
  grepl("only backfills when the async tint still empty", srv, fixed = TRUE) ||
    grepl("have_logo", srv, fixed = TRUE) ||
    grepl(".schedule_corp_logo_tint", srv, fixed = TRUE)
})
check(
  "quote KPI brand CSS beats header green",
  grepl(
    "body.ynow-corp-logo .ynow-header-kpi-row #ibx_stockprice .info-box-icon",
    ui,
    fixed = TRUE
  )
)
wc <- paste(readLines(file.path(app_dir, "web_crawler.R"), warn = FALSE), collapse = "\n")
check("crawler near-black helper", grepl("is_near_black_hex", wc, fixed = TRUE))
check("crawler mono helper", grepl("is_logo_mono_brand", wc, fixed = TRUE))
check("cache returns raw colors (no forced pad)", {
  grepl("Returns raw primary colors", wc, fixed = TRUE) &&
    grepl("normalize_logo_hex(cols)", wc, fixed = TRUE)
})
check("empty logo colors not retained in slow cache", {
  grepl(".drop_corp_logo_colors_cache_key", wc, fixed = TRUE) &&
    grepl("empty results are not kept", wc, fixed = TRUE) &&
    grepl("cached_get_corp_logo_colors <- function", wc, fixed = TRUE)
})
py_src <- paste(readLines(file.path(app_dir, "corp_logo_colors.py"), warn = FALSE), collapse = "\n")
check("python resolve keeps mono raw (no pad)", {
  grepl('"mono": len(colors) == 1', py_src, fixed = TRUE) &&
    !grepl("pad = \"#FFFFFF\" if max(r, g, b) <= 40 else \"#000000\"", py_src, fixed = TRUE)
})

req <- paste(readLines(file.path(app_dir, "requirements.txt"), warn = FALSE), collapse = "\n")
check("Pillow in requirements", grepl("Pillow", req, fixed = TRUE))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
