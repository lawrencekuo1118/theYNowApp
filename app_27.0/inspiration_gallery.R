# Inspiration Gallery（靈感藝廊）— homepage lazy slide + Sys Admin catalog.
# Bundled WebP under www/inspiration/; durable overrides in data/inspiration_gallery.json
# and uploads in data/inspiration_uploads/ (never runtime-write www/).
# Guest-writable: never. Mutators gated by caller via ynow_pts_sysadmin_on().

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

.ynow_insp_mem <- new.env(parent = emptyenv())
.ynow_insp_mem$catalog <- NULL
.ynow_insp_mem$res_ok <- FALSE

.ynow_insp_file <- function() {
  override <- Sys.getenv("YNOW_INSPIRATION_GALLERY_PATH", "")
  if (nzchar(override)) return(override)
  file.path("data", "inspiration_gallery.json")
}

.ynow_insp_uploads_dir <- function() {
  override <- Sys.getenv("YNOW_INSPIRATION_UPLOADS_DIR", "")
  if (nzchar(override)) return(override)
  file.path("data", "inspiration_uploads")
}

.ynow_insp_bundled_manifest <- function() {
  cands <- c(
    file.path("www", "inspiration", "manifest.json"),
    file.path("app_27.0", "www", "inspiration", "manifest.json")
  )
  for (p in cands) {
    if (is.character(p) && nzchar(p) && file.exists(p)) return(p)
  }
  ""
}

ynow_gallery_frequency <- function(show_probability = 1, rotation_interval_sec = 0L) {
  sp <- suppressWarnings(as.numeric(show_probability)[1])
  if (!is.finite(sp)) sp <- 1
  sp <- max(0, min(1, sp))
  ri <- suppressWarnings(as.integer(rotation_interval_sec)[1])
  if (!is.finite(ri) || ri < 0L) ri <- 0L
  list(show_probability = sp, rotation_interval_sec = ri)
}

ynow_gallery_image <- function(id, src, source = "bundled", bytes = NA_integer_,
                               enabled = TRUE, added_at = NA_character_) {
  list(
    id = as.character(id)[1],
    src = as.character(src)[1],
    source = as.character(source)[1],
    bytes = suppressWarnings(as.integer(bytes)[1]),
    enabled = isTRUE(enabled),
    added_at = as.character(added_at %||% "")[1]
  )
}

ynow_gallery_catalog <- function(frequency = ynow_gallery_frequency(), images = list()) {
  list(version = 1L, frequency = frequency, images = images)
}

ynow_gallery_roll_show <- function(freq, u = runif(1)) {
  sp <- as.numeric(freq$show_probability %||% 1)[1]
  if (!is.finite(sp)) sp <- 1
  uu <- as.numeric(u)[1]
  if (!is.finite(uu)) uu <- 0
  uu < sp
}

ynow_gallery_pick_next <- function(pool_ids, current_id = "", u = runif(1)) {
  ids <- unique(as.character(unlist(pool_ids, use.names = FALSE)))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) return(NA_character_)
  if (length(ids) == 1L) return(ids[[1]])
  cur <- as.character(current_id %||% "")[1]
  others <- if (nzchar(cur) && cur %in% ids) setdiff(ids, cur) else ids
  if (!length(others)) others <- ids
  uu <- as.numeric(u)[1]
  if (!is.finite(uu)) uu <- 0
  idx <- floor(uu * length(others)) + 1L
  if (idx < 1L) idx <- 1L
  if (idx > length(others)) idx <- length(others)
  others[[idx]]
}

.ynow_insp_parse_image <- function(raw) {
  if (!is.list(raw)) return(NULL)
  id <- as.character(raw$id %||% "")[1]
  src <- as.character(raw$src %||% "")[1]
  if (!nzchar(id) || !nzchar(src)) return(NULL)
  ynow_gallery_image(
    id = id,
    src = src,
    source = as.character(raw$source %||% "bundled")[1],
    bytes = raw$bytes %||% NA_integer_,
    enabled = if (is.null(raw$enabled)) TRUE else isTRUE(raw$enabled),
    added_at = as.character(raw$added_at %||% "")[1]
  )
}

.ynow_insp_parse_catalog <- function(raw) {
  if (!is.list(raw)) return(ynow_gallery_catalog())
  freq_raw <- raw$frequency %||% list()
  freq <- ynow_gallery_frequency(
    show_probability = freq_raw$show_probability %||% 1,
    rotation_interval_sec = freq_raw$rotation_interval_sec %||% 0L
  )
  imgs_raw <- raw$images %||% list()
  if (!is.list(imgs_raw)) imgs_raw <- list()
  imgs <- Filter(Negate(is.null), lapply(imgs_raw, .ynow_insp_parse_image))
  ynow_gallery_catalog(frequency = freq, images = imgs)
}

.ynow_insp_read_json <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  txt <- tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                  error = function(e) "")
  if (!nzchar(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
}

.ynow_insp_write_json <- function(path, obj) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  txt <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(txt, tmp, useBytes = TRUE)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    tryCatch(file.copy(tmp, path, overwrite = TRUE), error = function(e) NULL)
    unlink(tmp)
  }
  invisible(file.exists(path))
}

.ynow_insp_bundled_images <- function() {
  raw <- .ynow_insp_read_json(.ynow_insp_bundled_manifest())
  if (is.null(raw)) return(list())
  cat0 <- .ynow_insp_parse_catalog(raw)
  cat0$images
}

.ynow_insp_cleanup_orphans <- function(cat) {
  up_dir <- .ynow_insp_uploads_dir()
  keep <- list()
  for (im in cat$images) {
    if (!identical(im$source, "upload")) {
      keep[[length(keep) + 1L]] <- im
      next
    }
    fp <- file.path(up_dir, paste0(im$id, ".webp"))
    # Also accept path from src basename
    if (!file.exists(fp)) {
      bn <- basename(im$src)
      fp2 <- file.path(up_dir, bn)
      if (file.exists(fp2)) fp <- fp2
    }
    if (file.exists(fp)) {
      keep[[length(keep) + 1L]] <- im
    }
  }
  # A catalog read must not delete files. add_upload writes the WebP before the row is saved.
  cat$images <- keep
  cat
}

ynow_gallery_catalog_get <- function(force = FALSE) {
  if (!isTRUE(force) && is.list(.ynow_insp_mem$catalog)) {
    return(.ynow_insp_mem$catalog)
  }
  path <- .ynow_insp_file()
  raw <- .ynow_insp_read_json(path)
  bundled <- .ynow_insp_bundled_images()
  if (is.null(raw)) {
    cat0 <- ynow_gallery_catalog(
      frequency = ynow_gallery_frequency(),
      images = bundled
    )
  } else {
    cat0 <- .ynow_insp_parse_catalog(raw)
    # Merge: durable frequency + upload/disable overlay on bundled
    by_id <- list()
    for (im in bundled) by_id[[im$id]] <- im
    for (im in cat0$images) {
      if (identical(im$source, "upload")) {
        by_id[[im$id]] <- im
      } else if (!is.null(by_id[[im$id]])) {
        by_id[[im$id]]$enabled <- im$enabled
      } else {
        by_id[[im$id]] <- im
      }
    }
    # Durable-only frequency always wins when file exists
    cat0$images <- unname(by_id)
  }
  cat0 <- .ynow_insp_cleanup_orphans(cat0)
  .ynow_insp_mem$catalog <- cat0
  cat0
}

ynow_gallery_catalog_save <- function(cat) {
  cat <- .ynow_insp_cleanup_orphans(cat)
  # Persist only frequency + upload rows + disabled bundled overrides
  bundled_ids <- vapply(.ynow_insp_bundled_images(), function(x) x$id, character(1))
  store_imgs <- list()
  for (im in cat$images) {
    if (identical(im$source, "upload")) {
      store_imgs[[length(store_imgs) + 1L]] <- im
    } else if (im$id %in% bundled_ids && !isTRUE(im$enabled)) {
      store_imgs[[length(store_imgs) + 1L]] <- im
    }
  }
  payload <- list(
    version = 1L,
    frequency = cat$frequency,
    images = store_imgs
  )
  ok <- .ynow_insp_write_json(.ynow_insp_file(), payload)
  if (isTRUE(ok)) .ynow_insp_mem$catalog <- cat
  invisible(isTRUE(ok))
}

ynow_gallery_public_catalog <- function() {
  cat0 <- ynow_gallery_catalog_get()
  imgs <- Filter(function(im) isTRUE(im$enabled), cat0$images)
  list(
    version = cat0$version %||% 1L,
    frequency = cat0$frequency,
    images = lapply(imgs, function(im) {
      list(id = im$id, src = im$src, bytes = im$bytes %||% NA_integer_)
    })
  )
}

ynow_inspiration_ensure_resource_paths <- function() {
  if (isTRUE(.ynow_insp_mem$res_ok)) return(invisible(TRUE))
  up <- .ynow_insp_uploads_dir()
  dir.create(up, recursive = TRUE, showWarnings = FALSE)
  if (requireNamespace("shiny", quietly = TRUE)) {
    tryCatch(
      shiny::addResourcePath("inspiration-uploads", normalizePath(up, mustWork = FALSE)),
      error = function(e) NULL
    )
  }
  .ynow_insp_mem$res_ok <- TRUE
  invisible(TRUE)
}

ynow_gallery_set_frequency <- function(show_probability, rotation_interval_sec) {
  cat0 <- ynow_gallery_catalog_get(force = TRUE)
  cat0$frequency <- ynow_gallery_frequency(show_probability, rotation_interval_sec)
  ynow_gallery_catalog_save(cat0)
}

ynow_gallery_remove_image <- function(id) {
  id <- as.character(id %||% "")[1]
  if (!nzchar(id)) return(invisible(FALSE))
  cat0 <- ynow_gallery_catalog_get(force = TRUE)
  out <- list()
  for (im in cat0$images) {
    if (identical(im$id, id)) {
      if (identical(im$source, "upload")) {
        unlink(file.path(.ynow_insp_uploads_dir(), paste0(id, ".webp")))
        next
      }
      im$enabled <- FALSE
    }
    out[[length(out) + 1L]] <- im
  }
  cat0$images <- out
  ynow_gallery_catalog_save(cat0)
}

.ynow_insp_process_upload <- function(datapath, original_name = "") {
  datapath <- as.character(datapath %||% "")[1]
  if (!nzchar(datapath) || !file.exists(datapath)) return(NULL)
  ext <- tolower(tools::file_ext(as.character(original_name %||% "")[1]))
  if (!nzchar(ext)) ext <- tolower(tools::file_ext(datapath))
  if (!ext %in% c("png", "jpg", "jpeg", "gif", "webp")) return(NULL)
  ynow_inspiration_ensure_resource_paths()
  up_dir <- .ynow_insp_uploads_dir()
  dir.create(up_dir, recursive = TRUE, showWarnings = FALSE)
  id <- sprintf(
    "up%010d%04d",
    as.integer(as.numeric(Sys.time())) %% 1000000000L,
    sample.int(9999L, 1L)
  )
  dest <- file.path(up_dir, paste0(id, ".webp"))
  py <- Sys.which("python3")
  script <- file.path("scripts", "process_inspiration_gallery.py")
  if (!file.exists(script)) {
    script <- file.path("app_27.0", "scripts", "process_inspiration_gallery.py")
  }
  if (nzchar(py) && file.exists(script)) {
    tryCatch(
      system2(py, c(script, "--upload", datapath, dest), stdout = FALSE, stderr = FALSE),
      error = function(e) NULL
    )
  }
  if (!file.exists(dest) || !isTRUE(file.info(dest)$size > 0)) {
    if (file.exists(dest)) unlink(dest)
    if (!ext %in% c("webp", "png")) return(NULL)
    if (!isTRUE(file.copy(datapath, dest, overwrite = TRUE))) return(NULL)
  }
  if (!file.exists(dest)) return(NULL)
  ynow_gallery_image(
    id = id,
    src = paste0("inspiration-uploads/", id, ".webp"),
    source = "upload",
    bytes = file.info(dest)$size,
    enabled = TRUE,
    added_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )
}

ynow_gallery_add_upload <- function(datapath, original_name = "") {
  im <- .ynow_insp_process_upload(datapath, original_name)
  if (is.null(im)) return(invisible(NULL))
  cat0 <- ynow_gallery_catalog_get(force = TRUE)
  cat0$images[[length(cat0$images) + 1L]] <- im
  ynow_gallery_catalog_save(cat0)
  invisible(im)
}

#' Homepage shell: empty aspect box + credit + deferred boot (no img src).
ynow_inspiration_home_shell_ui <- function() {
  tagList(
    tags$div(
      id = "ynow_insp_gallery",
      class = "ynow-insp-gallery",
      role = "region",
      `aria-label` = "Inspiration",
      tags$button(
        type = "button",
        id = "ynow_insp_btn",
        class = "ynow-insp-btn",
        `aria-label` = "Show another inspiration image",
        tags$div(
          class = "ynow-insp-frame",
          tags$img(
            id = "ynow_insp_img",
            class = "ynow-insp-img",
            alt = "",
            decoding = "async",
            loading = "lazy"
          ),
          tags$span(
            id = "ynow_insp_credit",
            class = "ynow-insp-credit",
            `data-i18n` = "insp_credit",
            "© YNow Team"
          )
        )
      )
    ),
    tags$script(HTML("
(function () {
  if (window.__ynowInspBooted) return;
  window.__ynowInspBooted = true;
  var catalog = null;
  var currentId = '';
  var timer = null;
  function pool() {
    if (!catalog || !catalog.images) return [];
    return catalog.images.filter(function (x) { return x && x.id && x.src; });
  }
  function pick(avoid) {
    var p = pool();
    if (!p.length) return null;
    if (p.length === 1) return p[0];
    var others = p.filter(function (x) { return x.id !== avoid; });
    if (!others.length) others = p;
    return others[Math.floor(Math.random() * others.length)];
  }
  function show(item) {
    var img = document.getElementById('ynow_insp_img');
    var wrap = document.getElementById('ynow_insp_gallery');
    if (!img || !item) return;
    currentId = item.id;
    img.alt = '';
    img.src = item.src;
    if (wrap) wrap.classList.remove('ynow-insp-gallery--hidden');
  }
  function armRotation() {
    if (timer) { clearInterval(timer); timer = null; }
    var sec = catalog && catalog.frequency ? Number(catalog.frequency.rotation_interval_sec) : 0;
    if (!isFinite(sec) || sec <= 0) return;
    timer = setInterval(function () {
      var n = pick(currentId);
      if (n) show(n);
    }, sec * 1000);
  }
  function applyCatalog(c) {
    catalog = c || null;
    var wrap = document.getElementById('ynow_insp_gallery');
    if (!catalog) return;
    var sp = catalog.frequency ? Number(catalog.frequency.show_probability) : 1;
    if (!isFinite(sp)) sp = 1;
    if (Math.random() >= sp) {
      if (wrap) wrap.classList.add('ynow-insp-gallery--hidden');
      return;
    }
    if (wrap) wrap.classList.remove('ynow-insp-gallery--hidden');
    var first = pick('');
    if (first) show(first);
    armRotation();
  }
  function boot() {
    if (window.__ynowInspCatalog) {
      applyCatalog(window.__ynowInspCatalog);
      return;
    }
    try {
      fetch('inspiration/manifest.json', { credentials: 'same-origin' })
        .then(function (r) { return r.ok ? r.json() : null; })
        .then(function (j) { if (j) applyCatalog(j); })
        .catch(function () {});
    } catch (e) {}
  }
  function schedule() {
    if (window.requestIdleCallback) {
      requestIdleCallback(function () { boot(); }, { timeout: 2500 });
    } else {
      setTimeout(boot, 1);
    }
  }
  if (document.readyState === 'complete') schedule();
  else window.addEventListener('load', schedule);
  document.addEventListener('click', function (ev) {
    var t = ev.target && ev.target.closest ? ev.target.closest('#ynow_insp_btn') : null;
    if (!t) return;
    ev.preventDefault();
    var n = pick(currentId);
    if (n) show(n);
  });
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('ynowInspirationCatalog', function (msg) {
      window.__ynowInspCatalog = msg;
      applyCatalog(msg);
    });
  }
})();
"))
  )
}

#' Admin tab panel (stable host — fileInput lives in separate renderUI).
ynow_gallery_admin_tab_panel <- function(lab) {
  if (!is.function(lab)) {
    lab <- function(key, fb) fb
  }
  tabPanel(
    title = tags$span(
      id = "ynow_admin_insp_tab_label",
      lab("admin_insp_tab", "Inspiration Gallery")
    ),
    value = "inspiration",
    uiOutput("ynow_admin_insp_panel")
  )
}

#' Bind admin handlers + push catalog. Call once from ynow_server.R.
ynow_gallery_bind <- function(session, input, output, admin_on) {
  # .ynow_admin_ops_log is local to the Shiny server frame, not this module.
  srv <- parent.frame()
  for (i in seq_len(sys.nframe())) {
    e <- sys.frame(i)
    if (exists("ynow_pts_sysadmin_actor", envir = e, inherits = FALSE)) srv <- e
  }
  .audit <- function(action, detail) {
    if (exists(".ynow_admin_ops_log", envir = srv, inherits = FALSE)) {
      fn <- get(".ynow_admin_ops_log", envir = srv, inherits = FALSE)
      if (is.function(fn)) {
        tryCatch(fn(action, detail), error = function(e) NULL)
        return(invisible(TRUE))
      }
    }
    if (exists("ynow_admin_ops_append", mode = "function")) {
      tryCatch(
        ynow_admin_ops_append(action = action, detail = detail, actor = ""),
        error = function(e) NULL
      )
    }
    invisible(FALSE)
  }
  ynow_inspiration_ensure_resource_paths()
  tick <- shiny::reactiveVal(0L)

  .push <- function() {
    tryCatch(
      session$sendCustomMessage("ynowInspirationCatalog", ynow_gallery_public_catalog()),
      error = function(e) NULL
    )
  }

  shiny::observe({
    tick()
    .push()
  })

  output$ynow_admin_insp_panel <- shiny::renderUI({
    tick()
    on <- tryCatch(isTRUE(admin_on()), error = function(e) FALSE)
    loc <- tryCatch(
      normalize_ui_locale(shiny::isolate(ui_locale())),
      error = function(e) "en"
    )
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    if (!on) {
      return(tags$p(
        class = "text-muted",
        .lab("admin_insp_locked", "Enter system admin to manage the Inspiration Gallery.")
      ))
    }
    cat0 <- ynow_gallery_catalog_get()
    freq <- cat0$frequency
    rows <- lapply(cat0$images, function(im) {
      if (!isTRUE(im$enabled) && !identical(im$source, "upload")) return(NULL)
      tags$tr(
        tags$td(tags$code(im$id)),
        tags$td(im$source),
        tags$td(
          tags$img(src = im$src, style = "max-height:48px; max-width:80px; object-fit:contain;")
        ),
        tags$td(
          tags$button(
            type = "button",
            class = "btn btn-default btn-xs",
            onclick = sprintf(
              "if (window.Shiny) Shiny.setInputValue('ynow_insp_delete_id', '%s', {priority:'event'});",
              gsub("'", "", im$id)
            ),
            .lab("admin_insp_delete", "Remove")
          )
        )
      )
    })
    rows <- Filter(Negate(is.null), rows)
    tags$div(
      class = "ynow-admin-insp-pane",
      tags$h4(tags$span(id = "ynow_admin_insp_title", .lab("admin_insp_title", "Inspiration Gallery"))),
      tags$p(
        class = "text-muted",
        style = "font-size:12.5px;",
        tags$span(
          id = "ynow_admin_insp_help",
          .lab(
            "admin_insp_help",
            paste0(
              "Add or remove homepage gallery images. ",
              "Show probability is the chance the gallery mounts on a visit (0–1). ",
              "Rotation interval auto-advances when greater than 0 seconds; 0 means click-only."
            )
          )
        )
      ),
      shiny::fileInput(
        "ynow_insp_upload",
        label = tags$span(id = "ynow_admin_insp_upload_label", .lab("admin_insp_upload", "Upload image")),
        accept = c("image/png", "image/jpeg", "image/webp", "image/gif", ".png", ".jpg", ".jpeg", ".webp", ".gif"),
        buttonLabel = .lab("admin_insp_browse", "Browse…"),
        placeholder = .lab("admin_insp_nofile", "No file selected")
      ),
      tags$div(
        style = "display:flex; flex-wrap:wrap; gap:12px; align-items:flex-end; margin:10px 0;",
        shiny::numericInput(
          "ynow_insp_show_prob",
          label = tags$span(id = "ynow_admin_insp_prob_label", .lab("admin_insp_prob", "Show probability (0–1)")),
          value = freq$show_probability,
          min = 0, max = 1, step = 0.05,
          width = "160px"
        ),
        shiny::numericInput(
          "ynow_insp_rot_sec",
          label = tags$span(id = "ynow_admin_insp_rot_label", .lab("admin_insp_rot", "Rotation interval (sec)")),
          value = freq$rotation_interval_sec,
          min = 0, max = 3600, step = 1,
          width = "180px"
        ),
        shiny::actionButton(
          "ynow_insp_freq_save",
          label = tags$span(id = "ynow_admin_insp_freq_save_label", .lab("admin_insp_freq_save", "Save frequency")),
          class = "btn-danger btn-sm"
        )
      ),
      tags$div(
        style = "overflow-x:auto; max-height:40vh;",
        tags$table(
          class = "table table-condensed table-striped",
          tags$thead(tags$tr(
            tags$th(.lab("admin_insp_col_id", "Id")),
            tags$th(.lab("admin_insp_col_source", "Source")),
            tags$th(.lab("admin_insp_col_preview", "Preview")),
            tags$th("")
          )),
          tags$tbody(rows)
        )
      )
    )
  })

  shiny::observeEvent(input$ynow_insp_upload, {
    if (!tryCatch(isTRUE(admin_on()), error = function(e) FALSE)) return()
    f <- input$ynow_insp_upload
    if (is.null(f) || !nzchar(f$datapath %||% "")) return()
    im <- ynow_gallery_add_upload(f$datapath, f$name %||% "")
    if (!is.null(im)) .audit("insp.upload", paste0("id=", im$id))
    tick(isolate(tick()) + 1L)
    .push()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$ynow_insp_freq_save, {
    if (!tryCatch(isTRUE(admin_on()), error = function(e) FALSE)) return()
    ynow_gallery_set_frequency(input$ynow_insp_show_prob, input$ynow_insp_rot_sec)
    .audit(
      "insp.freq",
      sprintf("p=%s;rot=%s", input$ynow_insp_show_prob, input$ynow_insp_rot_sec)
    )
    tick(isolate(tick()) + 1L)
    .push()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$ynow_insp_delete_id, {
    if (!tryCatch(isTRUE(admin_on()), error = function(e) FALSE)) return()
    id <- as.character(input$ynow_insp_delete_id %||% "")[1]
    if (!nzchar(id)) return()
    ynow_gallery_remove_image(id)
    .audit("insp.delete", paste0("id=", id))
    tick(isolate(tick()) + 1L)
    .push()
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
