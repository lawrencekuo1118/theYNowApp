#!/usr/bin/env python3
"""
Cloud sync + memory guard for local development.

Watches OneDrive / Google Drive sync health and free RAM. When memory is
nearly exhausted, analyzes top consumers and reclaims safe caches / temp
space so File Provider sync is less likely to fail or stall.

Primary target: macOS (OneDrive Personal + Google Drive File Provider).
Linux is supported for memory monitoring and safe temp cleanup.

Usage:
  python3 tools/cloud_sync_guard.py              # watch loop
  python3 tools/cloud_sync_guard.py --once       # single check
  python3 tools/cloud_sync_guard.py --dry-run    # report only, no reclaim
"""

from __future__ import annotations

import argparse
import json
import os
import platform
import re
import shutil
import subprocess
import sys
import time
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable, List, Optional, Sequence, Tuple

HOME = Path.home()
DEFAULT_LOG = HOME / ".ynow_cloud_sync_guard.log"
DEFAULT_INTERVAL = 30
DEFAULT_MEM_WARN_PCT = 85.0          # used% of physical RAM
DEFAULT_AVAIL_WARN_MB = 1536         # absolute available floor
SAFE_TEMP_GLOBS = (
    "**/__pycache__",
    "**/.Rproj.user/**/cache",
    "**/.pytest_cache",
    "**/chromote-*/**",
)
PROTECTED_PROCESS_NAMES = {
    "Finder",
    "WindowServer",
    "kernel_task",
    "loginwindow",
    "OneDrive",
    "OneDrive File Provider",
    "Google Drive",
    "GoogleDrive",
    "Cursor",
    "Code",
    "RStudio",
    "rsession",
    "R",
}


@dataclass
class DriveStatus:
    name: str
    present: bool
    roots: List[str] = field(default_factory=list)
    process_running: bool = False
    process_names: List[str] = field(default_factory=list)
    pending_placeholders: int = 0
    recent_errors: List[str] = field(default_factory=list)
    healthy: bool = True
    notes: List[str] = field(default_factory=list)


@dataclass
class MemoryStatus:
    total_mb: float
    used_mb: float
    available_mb: float
    used_pct: float
    pressure: str  # normal | warn | critical
    top_processes: List[Tuple[str, float]] = field(default_factory=list)


@dataclass
class ReclaimAction:
    action: str
    detail: str
    freed_hint_mb: Optional[float] = None
    ok: bool = True


def utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def run_cmd(cmd: Sequence[str], timeout: float = 12.0) -> Tuple[int, str, str]:
    try:
        p = subprocess.run(
            list(cmd),
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
        return p.returncode, p.stdout or "", p.stderr or ""
    except FileNotFoundError:
        return 127, "", f"not found: {cmd[0]}"
    except subprocess.TimeoutExpired:
        return 124, "", f"timeout: {' '.join(cmd)}"


def is_macos() -> bool:
    return platform.system() == "Darwin"


def is_linux() -> bool:
    return platform.system() == "Linux"


def expand_cloud_roots() -> dict:
    """Discover OneDrive / Google Drive mount roots under File Provider or legacy paths."""
    roots = {"onedrive": [], "google_drive": []}
    cloud = HOME / "Library" / "CloudStorage"
    if cloud.is_dir():
        for child in sorted(cloud.iterdir()):
            name = child.name.lower()
            if not child.is_dir():
                continue
            if "onedrive" in name:
                roots["onedrive"].append(str(child))
            elif "google" in name:
                roots["google_drive"].append(str(child))

    legacy = [
        HOME / "OneDrive",
        HOME / "Google Drive",
        HOME / "GoogleDrive",
        HOME / "Library" / "CloudStorage" / "OneDrive-Personal",
    ]
    for path in legacy:
        if path.is_dir():
            key = "onedrive" if "onedrive" in path.name.lower() else "google_drive"
            s = str(path)
            if s not in roots[key]:
                roots[key].append(s)
    return roots


def process_running(patterns: Iterable[str]) -> Tuple[bool, List[str]]:
    code, out, _ = run_cmd(["ps", "-A", "-o", "comm="])
    if code != 0:
        return False, []
    lines = [ln.strip() for ln in out.splitlines() if ln.strip()]
    matched = []
    for pat in patterns:
        pat_l = pat.lower()
        for ln in lines:
            base = Path(ln).name.lower()
            if pat_l in base or pat_l in ln.lower():
                matched.append(ln)
                break
    return bool(matched), matched


def count_placeholder_hints(roots: Sequence[str], limit_scan: int = 4000) -> Tuple[int, List[str]]:
    """
    Heuristic: cloud placeholders / stuck sync markers.
    macOS File Provider often uses .*-icloud style or .partial / .tmp uploads.
    """
    notes: List[str] = []
    count = 0
    patterns = re.compile(
        r"(\.partial$|\.tmp$|\.download$|\.odoffline$|^\._|\.icloud$)",
        re.IGNORECASE,
    )
    scanned = 0
    for root in roots:
        root_path = Path(root)
        if not root_path.is_dir():
            continue
        try:
            for dirpath, dirnames, filenames in os.walk(root_path):
                # Skip deep vendor caches
                dirnames[:] = [
                    d
                    for d in dirnames
                    if d not in {".git", "node_modules", ".Trash", "Library"}
                ]
                for fn in filenames:
                    scanned += 1
                    if patterns.search(fn):
                        count += 1
                    if scanned >= limit_scan:
                        notes.append(f"scan capped at {limit_scan} files under {root}")
                        return count, notes
        except PermissionError as exc:
            notes.append(f"permission denied: {root} ({exc})")
        except OSError as exc:
            notes.append(f"walk error: {root} ({exc})")
    return count, notes


def recent_drive_log_errors(provider: str, max_lines: int = 80) -> List[str]:
    """Best-effort: pull recent Console-ish log lines if `log` is available (macOS)."""
    if not is_macos():
        return []
    predicate = {
        "onedrive": 'process == "OneDrive" OR process CONTAINS "OneDrive"',
        "google_drive": 'process CONTAINS "GoogleDrive" OR process CONTAINS "Google Drive"',
    }.get(provider)
    if not predicate:
        return []
    code, out, err = run_cmd(
        [
            "log",
            "show",
            "--last",
            "5m",
            "--style",
            "compact",
            "--predicate",
            predicate,
        ],
        timeout=20.0,
    )
    if code != 0:
        return []
    errors = []
    for line in out.splitlines()[-max_lines:]:
        low = line.lower()
        if any(k in low for k in ("error", "fail", "denied", "out of memory", "disk full", "quota")):
            errors.append(line.strip()[:240])
    return errors[-8:]


def probe_drive(name: str, process_patterns: Sequence[str], roots: Sequence[str]) -> DriveStatus:
    present = bool(roots)
    running, matched = process_running(process_patterns)
    pending, notes = count_placeholder_hints(roots) if roots else (0, ["no roots found"])
    errors = recent_drive_log_errors("onedrive" if "onedrive" in name.lower() else "google_drive")
    healthy = True
    if present and not running:
        healthy = False
        notes.append("cloud folder present but sync agent process not running")
    if pending >= 25:
        healthy = False
        notes.append(f"many placeholder/temp sync files ({pending})")
    if errors:
        # errors alone do not always mean unhealthy, but flag caution
        notes.append(f"{len(errors)} recent log error line(s)")
        if any("out of memory" in e.lower() or "disk full" in e.lower() for e in errors):
            healthy = False
    return DriveStatus(
        name=name,
        present=present,
        roots=list(roots),
        process_running=running,
        process_names=matched,
        pending_placeholders=pending,
        recent_errors=errors,
        healthy=healthy,
        notes=notes,
    )


def memory_status_macos() -> MemoryStatus:
    # page size
    code, out, _ = run_cmd(["pagesize"])
    page = int(out.strip()) if code == 0 and out.strip().isdigit() else 4096
    code, out, _ = run_cmd(["vm_stat"])
    stats = {}
    for line in out.splitlines():
        if ":" not in line:
            continue
        k, v = line.split(":", 1)
        digits = re.sub(r"[^\d]", "", v)
        if digits:
            stats[k.strip()] = int(digits)
    free = stats.get("Pages free", 0)
    speculative = stats.get("Pages speculative", 0)
    inactive = stats.get("Pages inactive", 0)
    purgeable = stats.get("Pages purgeable", 0)
    available_pages = free + speculative + inactive + purgeable
    available_mb = available_pages * page / (1024 * 1024)

    code, out, _ = run_cmd(["sysctl", "-n", "hw.memsize"])
    total_mb = int(out.strip()) / (1024 * 1024) if code == 0 and out.strip().isdigit() else 0.0
    used_mb = max(total_mb - available_mb, 0.0)
    used_pct = (used_mb / total_mb * 100.0) if total_mb else 0.0

    pressure = "normal"
    code, pout, _ = run_cmd(["memory_pressure"], timeout=8.0)
    text = (pout or "").lower()
    if "critical" in text:
        pressure = "critical"
    elif "warn" in text:
        pressure = "warn"
    elif used_pct >= 92 or available_mb < 512:
        pressure = "critical"
    elif used_pct >= 85 or available_mb < 1536:
        pressure = "warn"

    return MemoryStatus(
        total_mb=round(total_mb, 1),
        used_mb=round(used_mb, 1),
        available_mb=round(available_mb, 1),
        used_pct=round(used_pct, 1),
        pressure=pressure,
        top_processes=top_processes(12),
    )


def memory_status_linux() -> MemoryStatus:
    info = {}
    with open("/proc/meminfo", encoding="utf-8") as f:
        for line in f:
            parts = line.split()
            if len(parts) >= 2:
                info[parts[0].rstrip(":")] = int(parts[1])  # kB
    total_mb = info.get("MemTotal", 0) / 1024
    available_mb = info.get("MemAvailable", info.get("MemFree", 0)) / 1024
    used_mb = max(total_mb - available_mb, 0.0)
    used_pct = (used_mb / total_mb * 100.0) if total_mb else 0.0
    pressure = "normal"
    if used_pct >= 92 or available_mb < 512:
        pressure = "critical"
    elif used_pct >= 85 or available_mb < 1536:
        pressure = "warn"
    return MemoryStatus(
        total_mb=round(total_mb, 1),
        used_mb=round(used_mb, 1),
        available_mb=round(available_mb, 1),
        used_pct=round(used_pct, 1),
        pressure=pressure,
        top_processes=top_processes(12),
    )


def memory_status() -> MemoryStatus:
    if is_macos():
        return memory_status_macos()
    if is_linux():
        return memory_status_linux()
    # Fallback: unknown platform
    return MemoryStatus(0, 0, 0, 0, "normal", [])


def top_processes(limit: int = 10) -> List[Tuple[str, float]]:
    """Return [(name, rss_mb), ...] sorted by RSS desc."""
    if is_macos():
        code, out, _ = run_cmd(["ps", "-A", "-o", "rss=,comm="])
    else:
        code, out, _ = run_cmd(["ps", "-A", "-o", "rss=,comm="])
    if code != 0:
        return []
    rows: List[Tuple[str, float]] = []
    for line in out.splitlines():
        line = line.strip()
        if not line:
            continue
        parts = line.split(None, 1)
        if len(parts) != 2:
            continue
        try:
            rss_kb = float(parts[0])
        except ValueError:
            continue
        rows.append((parts[1], rss_kb / 1024.0))
    rows.sort(key=lambda x: x[1], reverse=True)
    return [(n, round(m, 1)) for n, m in rows[:limit]]


def dir_size_mb(path: Path) -> float:
    total = 0
    try:
        for root, _, files in os.walk(path):
            for fn in files:
                fp = Path(root) / fn
                try:
                    total += fp.stat().st_size
                except OSError:
                    pass
    except OSError:
        return 0.0
    return total / (1024 * 1024)


def safe_rmtree(path: Path) -> float:
    before = dir_size_mb(path) if path.exists() else 0.0
    try:
        if path.is_dir():
            shutil.rmtree(path, ignore_errors=True)
        elif path.is_file():
            path.unlink(missing_ok=True)
    except OSError:
        return 0.0
    return before


def reclaim_memory(project_root: Path, dry_run: bool) -> List[ReclaimAction]:
    actions: List[ReclaimAction] = []

    # 1) Analyze top consumers (report only; never kill protected / IDE / sync agents)
    tops = top_processes(15)
    heavy = [
        (n, mb)
        for n, mb in tops
        if mb >= 400 and Path(n).name not in PROTECTED_PROCESS_NAMES
        and not any(p.lower() in n.lower() for p in PROTECTED_PROCESS_NAMES)
    ]
    if heavy:
        detail = ", ".join(f"{n}={mb:.0f}MB" for n, mb in heavy[:8])
        actions.append(
            ReclaimAction(
                action="analyze_processes",
                detail=f"heavy non-protected processes: {detail}",
                ok=True,
            )
        )
    else:
        actions.append(
            ReclaimAction(
                action="analyze_processes",
                detail="no obvious safe-to-kill heavy processes; focusing on caches/temps",
                ok=True,
            )
        )

    # 2) Clear project / user safe temps
    candidates: List[Path] = [
        Path("/tmp"),
        Path(os.environ.get("TMPDIR", "/tmp")),
        HOME / ".cache" / "chromote",
        HOME / "Library" / "Caches" / "Microsoft Edge" / "Default" / "Cache"
        if is_macos()
        else Path("/dev/null"),
        HOME / "Library" / "Caches" / "com.microsoft.onedrive.pipeline"
        if is_macos()
        else Path("/dev/null"),
        HOME / "Library" / "Caches" / "Google" / "Chrome" / "Default" / "Code Cache"
        if is_macos()
        else Path("/dev/null"),
        project_root / ".Rproj.user",
    ]
    # Only clear known-safe cache subdirs, not entire Chrome profile
    safe_cache_dirs = []
    for c in candidates:
        if str(c) in {"/dev/null", str(Path("/dev/null"))}:
            continue
        if not c.exists():
            continue
        # Never wipe whole /tmp — only aged project-like dirs inside it
        if c in {Path("/tmp"), Path(os.environ.get("TMPDIR", "/tmp"))}:
            for child in c.iterdir() if c.is_dir() else []:
                name = child.name.lower()
                if any(
                    k in name
                    for k in (
                        "chromote",
                        "r_tmp",
                        "rscript",
                        "reticulate",
                        "ynow",
                        "shiny",
                        "pip-",
                        "pytest",
                    )
                ):
                    safe_cache_dirs.append(child)
            continue
        if c.name == ".Rproj.user":
            # only nested cache folders
            for cache_dir in c.glob("**/cache"):
                safe_cache_dirs.append(cache_dir)
            continue
        safe_cache_dirs.append(c)

    # Project __pycache__
    if project_root.is_dir():
        for pyc in project_root.rglob("__pycache__"):
            safe_cache_dirs.append(pyc)

    freed = 0.0
    for path in safe_cache_dirs:
        size = dir_size_mb(path)
        if size < 1.0 and path.name != "__pycache__":
            continue
        if dry_run:
            actions.append(
                ReclaimAction(
                    action="would_remove",
                    detail=str(path),
                    freed_hint_mb=round(size, 1),
                    ok=True,
                )
            )
            freed += size
            continue
        got = safe_rmtree(path)
        freed += got
        actions.append(
            ReclaimAction(
                action="removed",
                detail=str(path),
                freed_hint_mb=round(got, 1),
                ok=True,
            )
        )

    # 3) macOS purge (disk-backed inactive pages) — requires privileges; best-effort
    if is_macos():
        if dry_run:
            actions.append(
                ReclaimAction(
                    action="would_purge",
                    detail="memory_pressure high → would run `purge` (may need sudo)",
                    ok=True,
                )
            )
        else:
            code, out, err = run_cmd(["purge"], timeout=60.0)
            if code == 0:
                actions.append(
                    ReclaimAction(
                        action="purge",
                        detail="macOS purge completed",
                        ok=True,
                    )
                )
            else:
                # try sudo -n (non-interactive)
                code2, _, err2 = run_cmd(["sudo", "-n", "purge"], timeout=60.0)
                actions.append(
                    ReclaimAction(
                        action="purge",
                        detail=(
                            "macOS purge completed via sudo"
                            if code2 == 0
                            else f"purge skipped ({err or err2 or out or 'needs sudo'})"
                        ),
                        ok=(code2 == 0),
                    )
                )

    # 4) Linux drop caches if permitted
    if is_linux() and not dry_run:
        drop = Path("/proc/sys/vm/drop_caches")
        if drop.exists() and os.access(drop, os.W_OK):
            try:
                drop.write_text("3", encoding="utf-8")
                actions.append(ReclaimAction(action="drop_caches", detail="wrote 3 to drop_caches", ok=True))
            except OSError as exc:
                actions.append(ReclaimAction(action="drop_caches", detail=str(exc), ok=False))

    actions.append(
        ReclaimAction(
            action="summary",
            detail=f"approx reclaimable/freed cache estimate: {freed:.1f} MB",
            freed_hint_mb=round(freed, 1),
            ok=True,
        )
    )
    return actions


def log_line(log_path: Path, payload: dict) -> None:
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with log_path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(payload, ensure_ascii=False) + "\n")


def print_report(
    drives: Sequence[DriveStatus],
    mem: MemoryStatus,
    actions: Optional[Sequence[ReclaimAction]] = None,
) -> None:
    print(f"[{utc_now()}] memory: {mem.used_pct}% used | "
          f"{mem.available_mb:.0f} MB available / {mem.total_mb:.0f} MB | pressure={mem.pressure}")
    if mem.top_processes:
        top_s = ", ".join(f"{Path(n).name}={mb:.0f}MB" for n, mb in mem.top_processes[:5])
        print(f"  top RSS: {top_s}")
    for d in drives:
        flag = "OK" if d.healthy else "WARN"
        if not d.present:
            flag = "MISS"
        print(
            f"  [{flag}] {d.name}: present={d.present} process={d.process_running} "
            f"placeholders={d.pending_placeholders}"
        )
        for note in d.notes[:3]:
            print(f"         - {note}")
        for err in d.recent_errors[:2]:
            print(f"         ! {err}")
    if actions:
        print("  reclaim:")
        for a in actions:
            extra = f" (~{a.freed_hint_mb} MB)" if a.freed_hint_mb is not None else ""
            print(f"         - {a.action}: {a.detail}{extra}")


def should_reclaim(mem: MemoryStatus, warn_pct: float, avail_mb: float) -> bool:
    if mem.pressure in {"warn", "critical"}:
        return True
    if mem.used_pct >= warn_pct:
        return True
    if mem.available_mb and mem.available_mb < avail_mb:
        return True
    return False


def tick(args: argparse.Namespace) -> int:
    roots = expand_cloud_roots()
    drives = [
        probe_drive(
            "OneDrive",
            ["OneDrive", "OneDrive File Provider", "FileProvider"],
            roots["onedrive"],
        ),
        probe_drive(
            "Google Drive",
            ["Google Drive", "GoogleDrive", "GoogleDriveFS", "FileProvider"],
            roots["google_drive"],
        ),
    ]
    # FileProvider match is broad on macOS — refine process_running with name filter
    for d in drives:
        if d.process_running:
            refined = [
                p
                for p in d.process_names
                if ("onedrive" in p.lower() and "onedrive" in d.name.lower())
                or ("google" in p.lower() and "google" in d.name.lower())
            ]
            if refined:
                d.process_names = refined
                d.process_running = True
            elif d.present:
                # if only generic FileProvider matched, re-check specifically
                specific = ["OneDrive"] if "onedrive" in d.name.lower() else ["Google Drive", "GoogleDrive"]
                running, matched = process_running(specific)
                d.process_running = running
                d.process_names = matched
                if d.present and not running:
                    d.healthy = False
                    if "cloud folder present but sync agent process not running" not in d.notes:
                        d.notes.append("cloud folder present but sync agent process not running")

    mem = memory_status()
    actions: List[ReclaimAction] = []
    exit_code = 0

    unhealthy = [d for d in drives if d.present and not d.healthy]
    if unhealthy:
        exit_code = max(exit_code, 2)

    if should_reclaim(mem, args.mem_warn_pct, args.avail_warn_mb):
        exit_code = max(exit_code, 1)
        project_root = Path(args.project).resolve()
        actions = reclaim_memory(project_root, dry_run=args.dry_run)
        # After reclaim, sync warnings matter more — nudge user if drives unhealthy
        if unhealthy and not args.quiet:
            print(
                "  !! Memory pressure + unhealthy cloud sync — "
                "pause large writes until sync recovers.",
                file=sys.stderr,
            )

    print_report(drives, mem, actions or None)

    payload = {
        "ts": utc_now(),
        "memory": asdict(mem),
        "drives": [asdict(d) for d in drives],
        "actions": [asdict(a) for a in actions],
        "dry_run": args.dry_run,
    }
    log_line(Path(args.log), payload)
    return exit_code


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Monitor OneDrive/Google Drive sync + free RAM under pressure")
    p.add_argument("--once", action="store_true", help="Run a single check and exit")
    p.add_argument("--interval", type=int, default=DEFAULT_INTERVAL, help="Seconds between checks (watch mode)")
    p.add_argument("--mem-warn-pct", type=float, default=DEFAULT_MEM_WARN_PCT, help="Used RAM %% threshold")
    p.add_argument("--avail-warn-mb", type=float, default=DEFAULT_AVAIL_WARN_MB, help="Available RAM floor (MB)")
    p.add_argument("--dry-run", action="store_true", help="Analyze only; do not delete caches / purge")
    p.add_argument("--project", default=str(Path.cwd()), help="Project root for safe cache cleanup")
    p.add_argument("--log", default=str(DEFAULT_LOG), help="JSONL log path")
    p.add_argument("--quiet", action="store_true", help="Less stderr advisories")
    return p.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = parse_args(argv)
    if args.once:
        return tick(args)
    print(
        f"cloud_sync_guard watching every {args.interval}s "
        f"(mem_warn={args.mem_warn_pct}%, avail_floor={args.avail_warn_mb}MB, dry_run={args.dry_run})"
    )
    code = 0
    try:
        while True:
            code = tick(args)
            time.sleep(max(5, args.interval))
    except KeyboardInterrupt:
        print("\nstopped.")
        return code


if __name__ == "__main__":
    sys.exit(main())
