#!/usr/bin/env python3
"""Thin wrapper — canonical builder is build_us_industry_overlay.R.

Forwards argv to Rscript so muscle-memory `python3 scripts/build_…py` still works.
The R builder fetches Yahoo via get_yahoo_industry and fills industry_key via
resolve_industry_key_from_yahoo in one pass (no fill_* step needed).

Usage (from app_27.0/):
  python3 scripts/build_us_industry_overlay.py
  python3 scripts/build_us_industry_overlay.py --max-n 80

Prefer:
  Rscript scripts/build_us_industry_overlay.R
"""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

APP = Path(__file__).resolve().parents[1]
R_SCRIPT = Path(__file__).resolve().with_name("build_us_industry_overlay.R")


def main() -> int:
    rscript = shutil.which("Rscript")
    if not rscript:
        print(
            "Rscript not found on PATH. Install R or run:\n"
            "  Rscript scripts/build_us_industry_overlay.R",
            file=sys.stderr,
        )
        return 127
    if not R_SCRIPT.is_file():
        print(f"Missing canonical builder: {R_SCRIPT}", file=sys.stderr)
        return 1
    cmd = [rscript, str(R_SCRIPT), *sys.argv[1:]]
    print(
        "C-DUP-04: forwarding to canonical R builder →",
        " ".join(cmd),
        flush=True,
    )
    # Run with app_27.0 as cwd so relative data/ and source() paths resolve.
    return subprocess.call(cmd, cwd=str(APP), env=os.environ.copy())


if __name__ == "__main__":
    raise SystemExit(main())
