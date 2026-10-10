# test_corp_logo_colors.R — offline logo → corp-name tint helpers
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

# Source only the hex normalizer via a tiny local copy of the pure-R helper
# (full web_crawler.R pulls reticulate / caches — too heavy for this unit).
.normalize_logo_hex <- function(cols) {
  cols <- trimws(as.character(unlist(cols, use.names = FALSE)))
  cols <- cols[!is.na(cols) & nzchar(cols)]
  cols <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
  unique(toupper(head(cols, 2L)))
}

check("normalize keeps two hex", identical(
  .normalize_logo_hex(c("#e60a1e", "#00a0e6", "#ffffff")),
  c("#E60A1E", "#00A0E6")
))
check("normalize drops junk", identical(
  .normalize_logo_hex(c("red", "#GG0000", "#112233")),
  "#112233"
))
check("normalize empty", identical(.normalize_logo_hex(NULL), character(0)))

# Python extract from fixture (no network)
py <- file.path(app_dir, ".ynow_venv", "bin", "python")
if (!file.exists(py)) py <- Sys.which("python3")
fixture <- file.path(app_dir, "tests", "fixtures", "logo_red_blue.png")
check("fixture png exists", file.exists(fixture))

if (nzchar(py) && file.exists(fixture)) {
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
  cols <- .normalize_logo_hex(cols)
  check("fixture yields 1–2 colors", length(cols) >= 1L && length(cols) <= 2L)
  check("fixture primary is crimson-ish", {
    if (!length(cols)) FALSE else {
      h <- cols[[1]]
      r <- strtoi(substr(h, 2, 3), 16L)
      g <- strtoi(substr(h, 4, 5), 16L)
      b <- strtoi(substr(h, 6, 7), 16L)
      r > 150 && g < 80 && b < 100
    }
  })
  check("fixture secondary is blue-ish when present", {
    if (length(cols) < 2L) TRUE else {
      h <- cols[[2]]
      r <- strtoi(substr(h, 2, 3), 16L)
      g <- strtoi(substr(h, 4, 5), 16L)
      b <- strtoi(substr(h, 6, 7), 16L)
      b > 150 && r < 80
    }
  })
} else {
  cat("NOTE: skip python fixture extract (no venv python or fixture)\n")
}

# UI CSS hooks present
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE), collapse = "\n")
check("css logo-grad present", grepl("ynow-corpname-logo-grad", ui, fixed = TRUE))
check("css logo-solid present", grepl("ynow-corpname-logo-solid", ui, fixed = TRUE))

srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE), collapse = "\n")
check("server stores logo colors", grepl("corp_name_logo_colors", srv, fixed = TRUE))
check("server calls cached_get_corp_logo_colors", grepl("cached_get_corp_logo_colors", srv, fixed = TRUE))

req <- paste(readLines(file.path(app_dir, "requirements.txt"), warn = FALSE), collapse = "\n")
check("Pillow in requirements", grepl("Pillow", req, fixed = TRUE))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
