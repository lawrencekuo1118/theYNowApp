#!/usr/bin/env python3
"""
Cursor Cloud environment-only patch (NOT app code).

Yahoo Finance rate-limits the plain python-requests TLS fingerprint from
datacenter / cloud IPs (HTTP 429 "Too Many Requests"), while browser-
impersonated curl_cffi requests succeed. On shinyapps.io the app's default
yfinance session works fine, so the application itself needs no change.

In the Cursor Cloud VM the app is driven through reticulate's embedded Python,
where sitecustomize / usercustomize / .pth startup hooks are unavailable, so
the only reliable fix is to make the installed yfinance dependency default to a
curl_cffi browser-impersonation session. This edits the gitignored venv copy of
yfinance/data.py in place and is idempotent.

Usage:  <venv-python> scripts/ynow_cloud_yf_patch.py
"""
import pathlib
import sys

MARKER = "YNOW_CURL_CFFI_PATCH"

HELPER = '''

# ''' + MARKER + ''': default to curl_cffi browser impersonation to avoid
# datacenter-IP rate limiting (HTTP 429). Environment-only; app code unchanged.
def _ynow_default_session():
    try:
        from curl_cffi import requests as _creq
        return _creq.Session(impersonate="chrome")
    except Exception:
        import requests as _r
        return _r.Session()

'''


def main():
    try:
        import yfinance
    except Exception as e:  # noqa: BLE001
        print(f"yfinance not importable, skipping patch: {e}")
        return 0

    data_py = pathlib.Path(yfinance.__file__).parent / "data.py"
    if not data_py.exists():
        print(f"yfinance data.py not found at {data_py}, skipping")
        return 0

    src = data_py.read_text()
    if MARKER in src:
        print("yfinance already patched (curl_cffi default session)")
        return 0

    anchor = "from .exceptions import YFRateLimitError\n"
    target = "session or requests.Session()"
    if anchor not in src or target not in src:
        print("yfinance data.py structure unexpected; not patching")
        return 0

    src = src.replace(anchor, anchor + HELPER, 1)
    src = src.replace(target, "session or _ynow_default_session()", 1)
    data_py.write_text(src)
    print(f"patched {data_py} to use curl_cffi impersonation session")
    return 0


if __name__ == "__main__":
    sys.exit(main())
