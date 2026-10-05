#!/usr/bin/env bash
# Idempotent Cloud Agent bootstrap for theYNowApp (live line app_21.0).
# Installs CRAN R, r2u binary packages, and the app Python virtualenv.
set -euo pipefail

# Resolve the repo root from this script so the command is safe from any cwd.
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"
export DEBIAN_FRONTEND=noninteractive

if [[ "$(id -u)" -eq 0 ]]; then
  SUDO=""
else
  SUDO="sudo"
fi

$SUDO apt-get update -qq
$SUDO apt-get install -y --no-install-recommends ca-certificates gnupg wget

if [[ ! -s /usr/share/keyrings/r2u.gpg ]]; then
  $SUDO gpg --homedir /tmp --no-default-keyring \
    --keyring /usr/share/keyrings/r2u.gpg \
    --keyserver keyserver.ubuntu.com \
    --recv-keys A1489FE2AB99A21A 67C2D66C4B1D4339 51716619E084DAB9
fi

$SUDO tee /etc/apt/sources.list.d/r2u.sources >/dev/null <<'EOF'
Types: deb
URIs: https://r2u.stat.illinois.edu/ubuntu
Suites: noble
Components: main
Architectures: amd64 arm64
Signed-By: /usr/share/keyrings/r2u.gpg
EOF

$SUDO tee /etc/apt/sources.list.d/cran.sources >/dev/null <<'EOF'
Types: deb
URIs: https://cloud.r-project.org/bin/linux/ubuntu
Suites: noble-cran40/
Components:
Architectures: amd64 arm64
Signed-By: /usr/share/keyrings/r2u.gpg
EOF

$SUDO tee /etc/apt/preferences.d/99cranapt >/dev/null <<'EOF'
Package: *
Pin: release o=CRAN-Apt Project
Pin: release l=CRAN-Apt Packages
Pin-Priority: 700
EOF

$SUDO apt-get update -qq
$SUDO apt-get install -y --no-install-recommends \
  r-base \
  r-base-dev \
  pandoc \
  python3 \
  python3-venv \
  python3-pip \
  python3-dev \
  build-essential \
  pkg-config \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libxslt1-dev \
  r-cran-pacman \
  r-cran-tidyverse \
  r-cran-jsonlite \
  r-cran-ggrepel \
  r-cran-plotly \
  r-cran-dt \
  r-cran-rmarkdown \
  r-cran-knitr \
  r-cran-scales \
  r-cran-pagedown \
  r-cran-rvest \
  r-cran-xml2 \
  r-cran-reticulate \
  r-cran-shiny \
  r-cran-shinydashboard \
  r-cran-shinyjs \
  r-cran-shinycustomloader \
  r-cran-shinywidgets \
  r-cran-shinybs \
  r-cran-shinycssloaders \
  r-cran-memoise \
  r-cran-cachem \
  r-cran-ttr \
  r-cran-httr \
  r-cran-quantmod \
  r-cran-zoo \
  r-cran-htmlwidgets \
  r-cran-htmltools \
  r-cran-testthat \
  r-cran-rsconnect \
  r-cran-glue \
  r-cran-magrittr \
  r-cran-dplyr \
  r-cran-stringr \
  r-cran-purrr \
  r-cran-ggplot2

VENV="${ROOT}/app_21.0/.ynow_venv"
REQ="${ROOT}/app_21.0/requirements.txt"
if [[ ! -f "${REQ}" ]]; then
  echo "cloud-agent-install: missing ${REQ}" >&2
  exit 1
fi
if [[ ! -x "${VENV}/bin/python" ]]; then
  python3 -m venv "${VENV}"
fi
"${VENV}/bin/pip" install --disable-pip-version-check --upgrade pip
"${VENV}/bin/pip" install --disable-pip-version-check -r "${REQ}"

Rscript -e 'pkgs <- c("shiny","shinydashboard","tidyverse","reticulate","plotly","pacman","httr","quantmod","testthat","pagedown"); missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(missing)) { message("Missing R packages: ", paste(missing, collapse = ", ")); quit(status = 1) }'
"${VENV}/bin/python" -c 'import pandas, numpy, yfinance, requests, bs4, lxml, xlrd, curl_cffi'
echo "cloud-agent-install: ok"
