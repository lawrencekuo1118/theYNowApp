# test_download_login_gate.R — proprietary downloads require sign-in; public statements stay open.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK  ", label)
  } else {
    bad <<- bad + 1L
    message("FAIL ", label)
  }
}

source(file.path(app_dir, "auth_account.R"), local = TRUE)

check("gated id list includes PDF + snapshot + lab + capture", {
  ids <- ynow_gated_download_ids()
  all(c(
    "download_report", "download_snapshot", "download_param_restore",
    "download_defaults", "lab_im_download_report", "param_audit_pdf_go",
    "bblab-export_data", "bblab-export_chart"
  ) %in% ids)
})

check("public statement downloads listed", {
  ids <- ynow_public_download_ids()
  all(c("IS_download", "BS_download", "CF_download", "FS_download") %in% ids) &&
    !any(c("download_report", "download_snapshot") %in% ids)
})

check("guest denied when auth on", {
  isTRUE(ynow_guest_download_denied("", auth_enabled = TRUE))
})
check("signed-in allowed when auth on", {
  !isTRUE(ynow_guest_download_denied("user-1", auth_enabled = TRUE))
})
check("guest allowed when auth off", {
  !isTRUE(ynow_guest_download_denied("", auth_enabled = FALSE))
})

srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
bb <- paste(readLines(file.path(app_dir, "business_breakdown_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("server helper deny guest download", grepl(".ynow_deny_guest_download", srv, fixed = TRUE))
check("server gates snapshot", {
  grepl(
    "download_snapshot[\\s\\S]{0,500}\\.ynow_deny_guest_download",
    srv,
    perl = TRUE
  )
})
check("server gates param restore before write", {
  grepl(
    "download_param_restore[\\s\\S]{0,500}\\.ynow_deny_guest_download",
    srv,
    perl = TRUE
  )
})
check("server gates lab report", {
  grepl(
    "lab_im_download_report[\\s\\S]{0,400}\\.ynow_deny_guest_download",
    srv,
    perl = TRUE
  )
})
check("server gates param audit capture", {
  grepl(
    "param_audit_pdf_go[\\s\\S]{0,200}\\.ynow_deny_guest_download",
    srv,
    perl = TRUE
  )
})
check("server gates defaults CSV", {
  grepl(
    "download_defaults[\\s\\S]{0,400}\\.ynow_deny_guest_download",
    srv,
    perl = TRUE
  )
})
check("public IS/BS/CF still only access-lock (no guest deny)", {
  slice_handler <- function(name) {
    pat <- paste0("output\\$", name, " <- downloadHandler\\(")
    start <- regexpr(pat, srv, perl = TRUE)
    if (start < 1L) return("")
    chunk <- substr(srv, start, min(nchar(srv), start + 450L))
    chunk
  }
  is_block <- slice_handler("IS_download")
  bs_block <- slice_handler("BS_download")
  cf_block <- slice_handler("CF_download")
  nzchar(is_block) && nzchar(bs_block) && nzchar(cf_block) &&
    !grepl("ynow_deny_guest_download", is_block, fixed = TRUE) &&
    !grepl("ynow_deny_guest_download", bs_block, fixed = TRUE) &&
    !grepl("ynow_deny_guest_download", cf_block, fixed = TRUE) &&
    grepl("report:IS_download", is_block, fixed = TRUE) &&
    grepl("report:BS_download", bs_block, fixed = TRUE) &&
    grepl("report:CF_download", cf_block, fixed = TRUE)
})
check("bblab exports call guest deny", {
  grepl(".bblab_deny_guest_download", bb, fixed = TRUE) &&
    grepl("export_data[\\s\\S]{0,400}bblab_deny_guest_download", bb, perl = TRUE) &&
    grepl("export_chart[\\s\\S]{0,400}bblab_deny_guest_download", bb, perl = TRUE)
})
check("client click gate lists proprietary ids", {
  grepl("#download_snapshot", ui, fixed = TRUE) &&
    grepl("#lab_im_download_report", ui, fixed = TRUE) &&
    grepl("#param_audit_pdf_go", ui, fixed = TRUE) &&
    grepl("#bblab-export_data", ui, fixed = TRUE) &&
    grepl("ynowRequestAuthModal", ui, fixed = TRUE) &&
    !grepl("7302/ingest", ui, fixed = TRUE)
})
check("client does not gate public statement buttons", {
  start <- regexpr("Guest click gate for proprietary", ui, fixed = TRUE)
  if (start < 1L) return(FALSE)
  chunk <- substr(ui, start, min(nchar(ui), start + 1200L))
  grepl("#download_report", chunk, fixed = TRUE) &&
    grepl("#download_snapshot", chunk, fixed = TRUE) &&
    !grepl("#IS_download", chunk, fixed = TRUE) &&
    !grepl("#BS_download", chunk, fixed = TRUE) &&
    !grepl("#CF_download", chunk, fixed = TRUE)
})
check("locale en+zh download_need_login", {
  grepl('download_need_login = "Sign in to download this file."', loc, fixed = TRUE) &&
    grepl('download_need_login = "請先登入再下載此檔案。"', loc, fixed = TRUE)
})
check("server parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_server.R")), error = function(e) e), "expression"))
check("ui parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_ui.R")), error = function(e) e), "expression"))
check("bblab parses", inherits(tryCatch(parse(file.path(app_dir, "business_breakdown_module.R")), error = function(e) e), "expression"))
check("auth parses", inherits(tryCatch(parse(file.path(app_dir, "auth_account.R")), error = function(e) e), "expression"))

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
