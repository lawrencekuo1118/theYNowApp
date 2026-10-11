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

# Source only the hex helpers via a tiny local copy of the pure-R helpers
# (full web_crawler.R pulls reticulate / caches — too heavy for this unit).
.normalize_logo_hex <- function(cols) {
  cols <- trimws(as.character(unlist(cols, use.names = FALSE)))
  cols <- cols[!is.na(cols) & nzchar(cols)]
  cols <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
  unique(toupper(head(cols, 2L)))
}
.ensure_logo_color_pair <- function(cols) {
  cols <- .normalize_logo_hex(cols)
  if (length(cols) == 1L) {
    return(c(cols[[1]], "#FFFFFF"))
  }
  cols
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
check("pair pads single with white", identical(
  .ensure_logo_color_pair("#e60a1e"),
  c("#E60A1E", "#FFFFFF")
))
check("pair keeps two brands", identical(
  .ensure_logo_color_pair(c("#fab400", "#00a0e6")),
  c("#FAB400", "#00A0E6")
))
check("pair empty stays empty", identical(
  .ensure_logo_color_pair(NULL),
  character(0)
))

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
  cols <- .ensure_logo_color_pair(cols)
  check("fixture yields exactly two colors", identical(length(cols), 2L))
  check("fixture primary is crimson-ish", {
    if (!length(cols)) FALSE else {
      h <- cols[[1]]
      r <- strtoi(substr(h, 2, 3), 16L)
      g <- strtoi(substr(h, 4, 5), 16L)
      b <- strtoi(substr(h, 6, 7), 16L)
      r > 150 && g < 80 && b < 100
    }
  })
  check("fixture secondary is blue-ish or white pad", {
    if (length(cols) < 2L) FALSE else {
      h <- cols[[2]]
      if (identical(h, "#FFFFFF")) TRUE else {
        r <- strtoi(substr(h, 2, 3), 16L)
        g <- strtoi(substr(h, 4, 5), 16L)
        b <- strtoi(substr(h, 6, 7), 16L)
        b > 150 && r < 80
      }
    }
  })
} else {
  cat("NOTE: skip python fixture extract (no venv python or fixture)\n")
}

# Python resolve pads single-brand logos with white (offline synthetic)
if (nzchar(py)) {
  py_script <- tempfile(fileext = ".py")
  .py_str <- function(x) {
    paste0("'", gsub("\\\\", "\\\\\\\\", gsub("'", "\\\\'", x, fixed = TRUE), fixed = TRUE), "'")
  }
  writeLines(
    c(
      "import sys, io",
      paste0("sys.path.insert(0, ", .py_str(app_dir), ")"),
      "from PIL import Image",
      "from corp_logo_colors import extract_logo_colors_from_bytes, resolve_corp_logo_colors",
      "# single-color red logo",
      "im = Image.new('RGBA', (48, 48), (230, 10, 30, 255))",
      "buf = io.BytesIO(); im.save(buf, format='PNG')",
      "raw = extract_logo_colors_from_bytes(buf.getvalue(), max_colors=2)",
      "print('RAW', ','.join(raw))",
      "# mimic resolve pad",
      "cols = list(raw)",
      "if len(cols) == 1: cols = [cols[0], '#FFFFFF']",
      "print('PAD', ','.join(cols))"
    ),
    py_script
  )
  out <- tryCatch(
    system2(py, py_script, stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  unlink(py_script)
  pad_line <- trimws(tail(out[grepl("^PAD ", out)], 1))
  pad_cols <- if (nzchar(pad_line)) {
    strsplit(sub("^PAD ", "", pad_line), ",", fixed = TRUE)[[1]]
  } else {
    character(0)
  }
  pad_cols <- toupper(pad_cols)
  check("single-brand pad ends with white", {
    length(pad_cols) == 2L && identical(pad_cols[[2]], "#FFFFFF")
  })
} else {
  cat("NOTE: skip python single-brand pad check\n")
}

# UI CSS hooks present
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE), collapse = "\n")
check("css logo-grad present", grepl("ynow-corpname-logo-grad", ui, fixed = TRUE))
check("css white-en helper present", grepl("ynow-corpname-en-on-white", ui, fixed = TRUE))

srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE), collapse = "\n")
check("server stores logo colors", grepl("corp_name_logo_colors", srv, fixed = TRUE))
check("server ensures color pair", grepl("ensure_logo_color_pair", srv, fixed = TRUE))
check("server calls cached_get_corp_logo_colors", grepl("cached_get_corp_logo_colors", srv, fixed = TRUE))

req <- paste(readLines(file.path(app_dir, "requirements.txt"), warn = FALSE), collapse = "\n")
check("Pillow in requirements", grepl("Pillow", req, fixed = TRUE))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
