# ==========================================
# setup_feedback.R — Feedback → GitHub Issue helpers
# Extracted from setup.R (C-STRUCT-04). Sourced by setup.R.
# ==========================================

# =========================================================
# 💬 意見區：提交 GitHub Issue（YNOW_FEEDBACK_GITHUB_TOKEN）
# =========================================================
.ynow_feedback_github_repo <- function() {
  repo <- Sys.getenv("YNOW_FEEDBACK_GITHUB_REPO", unset = "lawrencekuo1118/theYNowApp")
  repo <- trimws(as.character(repo)[1])
  if (!nzchar(repo) || !grepl("^[^/]+/[^/]+$", repo)) {
    return("lawrencekuo1118/theYNowApp")
  }
  repo
}

.ynow_feedback_github_token <- function() {
  tok <- Sys.getenv("YNOW_FEEDBACK_GITHUB_TOKEN", unset = "")
  if (!nzchar(tok)) tok <- Sys.getenv("GITHUB_TOKEN", unset = "")
  if (!nzchar(tok)) tok <- Sys.getenv("GH_TOKEN", unset = "")
  trimws(as.character(tok)[1])
}

#' Ensure a label exists on the feedback repo (best-effort; ignore failures).
.ynow_ensure_github_label <- function(repo, name, color = "0E8A16", token) {
  if (!nzchar(token) || !nzchar(name)) return(invisible(FALSE))
  if (!requireNamespace("httr", quietly = TRUE)) return(invisible(FALSE))
  url <- sprintf("https://api.github.com/repos/%s/labels", repo)
  res <- tryCatch(
    httr::POST(
      url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/vnd.github+json",
        "X-GitHub-Api-Version" = "2022-11-28"
      ),
      httr::user_agent("TheYNowApp-feedback"),
      body = list(name = name, color = color, description = "User feedback from The YNow App"),
      encode = "json",
      httr::timeout(15)
    ),
    error = function(e) NULL
  )
  if (is.null(res)) return(invisible(FALSE))
  code <- httr::status_code(res)
  invisible(code %in% c(201L, 422L)) # created or already exists
}

#' Create a GitHub Issue from the in-app feedback form.
#' @return list(ok=, html_url=, number=, message=)
.ynow_create_feedback_issue <- function(title, body, labels = character(0)) {
  token <- .ynow_feedback_github_token()
  repo <- .ynow_feedback_github_repo()
  if (!nzchar(token)) {
    return(list(
      ok = FALSE,
      html_url = NA_character_,
      number = NA_integer_,
      message = paste0(
        "尚未設定環境變數 YNOW_FEEDBACK_GITHUB_TOKEN（issues:write）。",
        "無法透過 API 建立 Issue。"
      )
    ))
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(
      ok = FALSE, html_url = NA_character_, number = NA_integer_,
      message = "伺服器缺少 httr 套件，無法呼叫 GitHub API。"
    ))
  }
  title <- trimws(as.character(title %||% "")[1])
  body <- as.character(body %||% "")[1]
  if (!nzchar(title)) {
    return(list(ok = FALSE, html_url = NA_character_, number = NA_integer_,
                message = "標題不可空白。"))
  }
  labels <- unique(c("feedback", as.character(labels)))
  labels <- labels[nzchar(labels)]
  for (lab in labels) {
    .ynow_ensure_github_label(repo, lab, token = token)
  }
  url <- sprintf("https://api.github.com/repos/%s/issues", repo)
  res <- tryCatch(
    httr::POST(
      url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/vnd.github+json",
        "X-GitHub-Api-Version" = "2022-11-28"
      ),
      httr::user_agent("TheYNowApp-feedback"),
      body = list(title = title, body = body, labels = as.list(labels)),
      encode = "json",
      httr::timeout(30)
    ),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(ok = FALSE, html_url = NA_character_, number = NA_integer_,
                message = paste0("連線失敗：", conditionMessage(res))))
  }
  code <- httr::status_code(res)
  parsed <- tryCatch(httr::content(res, as = "parsed", type = "application/json"),
                     error = function(e) NULL)
  if (code >= 200 && code < 300 && !is.null(parsed)) {
    return(list(
      ok = TRUE,
      html_url = as.character(parsed$html_url %||% NA_character_),
      number = suppressWarnings(as.integer(parsed$number %||% NA_integer_)),
      message = "ok"
    ))
  }
  # Retry without labels if label validation failed
  if (code == 422L && length(labels) > 0) {
    res2 <- tryCatch(
      httr::POST(
        url,
        httr::add_headers(
          Authorization = paste("Bearer", token),
          Accept = "application/vnd.github+json",
          "X-GitHub-Api-Version" = "2022-11-28"
        ),
        httr::user_agent("TheYNowApp-feedback"),
        body = list(
          title = title,
          body = paste0(body, "\n\n_Labels (requested):_ ", paste(labels, collapse = ", "))
        ),
        encode = "json",
        httr::timeout(30)
      ),
      error = function(e) e
    )
    if (!inherits(res2, "error")) {
      code2 <- httr::status_code(res2)
      parsed2 <- tryCatch(httr::content(res2, as = "parsed", type = "application/json"),
                          error = function(e) NULL)
      if (code2 >= 200 && code2 < 300 && !is.null(parsed2)) {
        return(list(
          ok = TRUE,
          html_url = as.character(parsed2$html_url %||% NA_character_),
          number = suppressWarnings(as.integer(parsed2$number %||% NA_integer_)),
          message = "ok (created without labels)"
        ))
      }
    }
  }
  err_msg <- if (!is.null(parsed$message)) parsed$message else paste("HTTP", code)
  list(ok = FALSE, html_url = NA_character_, number = NA_integer_, message = as.character(err_msg))
}
