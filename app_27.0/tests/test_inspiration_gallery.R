# Inspiration Gallery: catalog, home shell, admin tab, locale, lazy boot.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
Sys.setenv(YNOW_INSPIRATION_GALLERY_FORCE_FILE = "1")

.fail <- 0L
.pass <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) {
    .pass <<- .pass + 1L
    cat("PASS:", label, "\n")
  } else {
    .fail <<- .fail + 1L
    cat("FAIL:", label, "\n")
  }
  invisible(ok)
}

app_dir <- normalizePath("..")
tmpdir <- tempfile("ynow-insp-")
dir.create(tmpdir, recursive = TRUE)
Sys.setenv(YNOW_INSPIRATION_GALLERY_PATH = file.path(tmpdir, "inspiration_gallery.json"))
Sys.setenv(YNOW_INSPIRATION_UPLOADS_DIR = file.path(tmpdir, "uploads"))

# Work from app_dir so www/inspiration resolves.
old_wd <- getwd()
setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)

source("inspiration_gallery.R", local = FALSE)

check("pick_next avoids immediate repeat when pool > 1", {
  ids <- c("a", "b", "c")
  n1 <- ynow_gallery_pick_next(ids, current_id = "a", u = 0)
  n1 != "a" && n1 %in% ids
})

check("roll_show respects probability", {
  f <- ynow_gallery_frequency(show_probability = 0.25, rotation_interval_sec = 0)
  isTRUE(ynow_gallery_roll_show(f, u = 0.1)) &&
    !isTRUE(ynow_gallery_roll_show(f, u = 0.9))
})

check("catalog get merges bundled manifest", {
  cat0 <- ynow_gallery_catalog_get(force = TRUE)
  is.list(cat0$frequency) &&
    length(cat0$images) >= 1L &&
    all(vapply(cat0$images, function(im) nzchar(im$id) && nzchar(im$src), logical(1)))
})

check("frequency save roundtrip", {
  ok <- ynow_gallery_set_frequency(0.5, 12L)
  cat0 <- ynow_gallery_catalog_get(force = TRUE)
  isTRUE(ok) &&
    isTRUE(all.equal(cat0$frequency$show_probability, 0.5)) &&
    identical(as.integer(cat0$frequency$rotation_interval_sec), 12L)
})

check("public catalog only enabled images", {
  pub <- ynow_gallery_public_catalog()
  is.list(pub$images) &&
    !is.null(pub$frequency$show_probability)
})

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE), collapse = "\n")
srv_src <- paste(readLines("ynow_server.R", warn = FALSE), collapse = "\n")
loc_src <- paste(readLines("ui_locale.R", warn = FALSE), collapse = "\n")
glb <- paste(readLines("global.R", warn = FALSE), collapse = "\n")

check("global sources inspiration_gallery.R", {
  grepl('source\\("inspiration_gallery\\.R"', glb)
})

check("home shell between auth and about", {
  legal <- regmatches(
    ui_src,
    regexpr('class = "ynow-home-legal"[\\s\\S]*?tags\\$script', ui_src, perl = TRUE)
  )
  if (!length(legal)) return(FALSE)
  block <- legal[[1]]
  i_auth <- regexpr("ynow_auth_home_ui", block, fixed = TRUE)[1]
  i_gal <- regexpr("ynow_inspiration_home_shell_ui", block, fixed = TRUE)[1]
  i_about <- regexpr('id = "ynow_home_legal_about"', block, fixed = TRUE)[1]
  i_disc <- regexpr('id = "ynow_home_legal"', block, fixed = TRUE)[1]
  i_auth > 0 && i_gal > 0 && i_about > 0 && i_disc > 0 &&
    i_auth < i_gal && i_gal < i_about && i_about < i_disc &&
    !grepl("ynow-home-legal-sep", block, fixed = TRUE)
})

check("responsive CSS aspect-ratio and desktop two-card width", {
  grepl("aspect-ratio:\\s*4\\s*/\\s*3", ui_src) &&
    grepl("ynow-insp-gallery", ui_src, fixed = TRUE) &&
    grepl("calc\\(\\(100% - 20px\\) \\* 2 / 3 \\+ 10px\\)", ui_src)
})

check("lazy boot: static img has no src attribute set", {
  mod <- paste(readLines("inspiration_gallery.R", warn = FALSE), collapse = "\n")
  grepl('id = "ynow_insp_img"', mod, fixed = TRUE) &&
    grepl("requestIdleCallback", mod, fixed = TRUE) &&
    grepl("inspiration/manifest.json", mod, fixed = TRUE) &&
    !grepl('id = "ynow_insp_img"[\\s\\S]{0,160}src\\s*=', mod, perl = TRUE)
})

check("credit overlay id present", {
  mod <- paste(readLines("inspiration_gallery.R", warn = FALSE), collapse = "\n")
  grepl('id = "ynow_insp_credit"', mod, fixed = TRUE) &&
    grepl("insp_credit", loc_src, fixed = TRUE) &&
    grepl("YNow Team 版權所有", loc_src, fixed = TRUE) &&
    grepl("© YNow Team", loc_src, fixed = TRUE)
})

check("admin tab helper wired in both sysadmin branches", {
  mod <- paste(readLines("inspiration_gallery.R", warn = FALSE), collapse = "\n")
  grepl("ynow_gallery_admin_tab_panel", srv_src, fixed = TRUE) &&
    grepl("ynow_gallery_bind", srv_src, fixed = TRUE) &&
    grepl("ynow_admin_insp_panel", mod, fixed = TRUE) &&
    length(gregexpr("ynow_gallery_admin_tab_panel", srv_src, fixed = TRUE)[[1]]) >= 2L
})

check("locale admin_insp_tab en + zh-TW", {
  grepl("admin_insp_tab = \"Inspiration Gallery\"", loc_src, fixed = TRUE) &&
    grepl("admin_insp_tab = \"靈感藝廊\"", loc_src, fixed = TRUE)
})

check("audit actions include insp.*", {
  src <- paste(readLines("admin_ops_audit.R", warn = FALSE), collapse = "\n")
  grepl("insp.upload", src, fixed = TRUE) &&
    grepl("insp.delete", src, fixed = TRUE) &&
    grepl("insp.freq", src, fixed = TRUE)
})

check("bundled webp pool present", {
  length(list.files("www/inspiration", pattern = "\\.webp$")) >= 5L &&
    file.exists("www/inspiration/manifest.json")
})

cat("\ninspiration_gallery tests: ", .pass, " passed, ", .fail, " failed\n", sep = "")
if (.fail > 0L) quit(status = 1L)
