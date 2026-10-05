# Early boot guard for shinyapps / Posit Connect.
# Runs before app.R. Without a bundled .ynow_venv, reticulate 1.4x defaults to an
# ephemeral uv env and may download CPython (~33MiB) before Shiny can Listen.
# Local cloud/dev clones keep .ynow_venv and skip this block.
local({
  venv_py <- file.path(getwd(), ".ynow_venv", "bin", "python")
  if (!file.exists(venv_py)) {
    Sys.setenv(
      UV_PYTHON_DOWNLOADS = "never",
      RETICULATE_USE_MANAGED_VENV = "false"
    )
  }
})
