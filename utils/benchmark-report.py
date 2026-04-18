#!/usr/bin/env python3

"""Run benchmark subsystems and render static HTML reports."""

from __future__ import annotations

import argparse
import datetime as dt
import html
import json
import math
import os
import platform
import re
import resource
import shlex
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Iterable


ROOT = Path(__file__).resolve().parent.parent
CSS_PATH = ROOT / "web/benchmarks/dist/report.css"
CSV_RE = re.compile(r"^\+!CSVLINE!\+([^,]+),([^,]+),(.+)$")
NUMERIC_RE = re.compile(r"^-?(?:\d+(?:\.\d*)?|\.\d+)$")

R7RS_SYSTEM = "arete"
ARETE_SYSTEM = "arete-workload"
DEFAULTS = {
    "r7rs": {
        "report": ROOT / "web/benchmarks/reports/r7rs.html",
        "log": ROOT / "web/benchmarks/reports/r7rs.log",
        "title": "R7RS Benchmarks",
        "subtitle": (
            "Static HTML report generated from the existing Arete R7RS benchmark "
            "runner. This subsystem tracks the vendored ecraven benchmark suite."
        ),
        "selection_label": "Bench selection",
        "selection_default": ["perf"],
        "selection_help": "Benchmark names or groups. Defaults to 'perf'.",
        "table_copy": (
            "The table below is driven directly from <code>+!CSVLINE!+</code> rows "
            "emitted by the existing R7RS harness. The full captured log is "
            "available at <a href=\"{log_href}\">{log_name}</a>."
        ),
        "cpu_detail": "seconds per benchmark",
    },
    "arete": {
        "report": ROOT / "web/benchmarks/reports/arete.html",
        "log": ROOT / "web/benchmarks/reports/arete.log",
        "title": "Arete Workloads",
        "subtitle": (
            "Static HTML report for project-native workloads that exercise Arete's "
            "own bootstrap path. These runs cover bootstrapping the system and the "
            "heavier psyntax bootstrap experiment."
        ),
        "selection_label": "Workload selection",
        "selection_default": ["all"],
        "selection_help": "Workload names. Defaults to 'all'.",
        "table_copy": (
            "The table below is driven directly from <code>+!CSVLINE!+</code> rows "
            "emitted by the Arete workload runner. The full captured log is "
            "available at <a href=\"{log_href}\">{log_name}</a>."
        ),
        "cpu_detail": "seconds per run",
    },
}

ARETE_WORKLOADS = {
    "boot": {
        "argv": [str(ROOT / "bin/arete"), "boot.scm"],
        "description": "Bootstraps Arete from source via boot.scm.",
    },
    "bootstrap-and-psyntax": {
        "argv": [str(ROOT / "bin/arete"), "bootstrap-and-psyntax.scm"],
        "description": "Bootstraps Arete and then loads and expands psyntax.",
    },
}
ARETE_ALIASES = {
    "psyntax": "bootstrap-and-psyntax",
}


def shell_join(parts: Iterable[str]) -> str:
    return " ".join(shlex.quote(part) for part in parts)


def ensure_parent(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)


def format_duration(value: float) -> str:
    if value >= 1:
        seconds = f"{value:.3f}".rstrip("0").rstrip(".")
        return f"{seconds}s"

    milliseconds = value * 1000.0
    millis = f"{milliseconds:.3f}".rstrip("0").rstrip(".")
    return f"{millis}ms"


def classify_result(raw_value: str) -> tuple[str, str, float | None]:
    if NUMERIC_RE.fullmatch(raw_value):
        seconds = float(raw_value)
        return ("PASS", format_duration(seconds), seconds)
    if raw_value == "INCORRECT":
        return ("INCORRECT", raw_value, None)
    if raw_value == "ULIMITKILLED":
        return ("TIMEOUT", raw_value, None)
    return ("FAIL", raw_value, None)


def canonical_name(display_name: str) -> str:
    return display_name.split(":", 1)[0]


def parse_log(log_text: str, expected_system: str) -> list[dict[str, object]]:
    rows_by_name: dict[str, dict[str, object]] = {}
    for line in log_text.splitlines():
        match = CSV_RE.match(line.strip())
        if not match:
            continue
        system, display_name, raw_value = match.groups()
        if system != expected_system:
            continue
        status, display_value, seconds = classify_result(raw_value)
        rows_by_name[display_name] = {
            "system": system,
            "display_name": display_name,
            "benchmark": canonical_name(display_name),
            "raw_value": raw_value,
            "status": status,
            "display_value": display_value,
            "seconds": seconds,
            "milliseconds": seconds * 1000.0 if isinstance(seconds, float) else None,
        }
    rows = list(rows_by_name.values())
    rows.sort(key=lambda row: str(row["display_name"]))
    return rows


def summarize(rows: list[dict[str, object]]) -> dict[str, object]:
    counts = {"PASS": 0, "TIMEOUT": 0, "INCORRECT": 0, "FAIL": 0}
    passed_seconds: list[float] = []
    for row in rows:
        status = str(row["status"])
        counts[status] = counts.get(status, 0) + 1
        seconds = row["seconds"]
        if isinstance(seconds, float):
            passed_seconds.append(seconds)
    return {
        "total": len(rows),
        "counts": counts,
        "pass_count": len(passed_seconds),
        "pass_seconds_total": sum(passed_seconds),
        "pass_seconds_median": statistics.median(passed_seconds) if passed_seconds else None,
        "fastest": min(passed_seconds) if passed_seconds else None,
        "slowest": max(passed_seconds) if passed_seconds else None,
    }


def nested_get(mapping: dict[str, object], path: tuple[str, ...]) -> object | None:
    current: object = mapping
    for key in path:
        if not isinstance(current, dict) or key not in current:
            return None
        current = current[key]
    return current


def format_pct_change(delta_pct: float | None) -> str:
    if delta_pct is None:
        return "n/a"
    return f"{delta_pct:+.2f}%"


def build_payload(
    *,
    subsystem: str,
    rows: list[dict[str, object]],
    report_path: Path,
    log_path: Path,
    selection_args: list[str],
    cpu_limit: str,
    generated_at: dt.datetime,
    command: str,
    extra: dict[str, object] | None = None,
) -> dict[str, object]:
    payload = {
        "subsystem": subsystem,
        "generated_at": generated_at.isoformat(),
        "report": str(report_path),
        "log": str(log_path),
        "selection": selection_args,
        "cpu_limit": cpu_limit,
        "command": command,
        "summary": summarize(rows),
        "rows": rows,
    }
    if extra:
        payload.update(extra)
    return payload


def status_class(status: str) -> str:
    if status == "PASS":
        return "status-chip status-pass"
    if status == "INCORRECT":
        return "status-chip status-incorrect"
    return "status-chip status-fail"


def pill_class(kind: str) -> str:
    return {
        "PASS": "pill pill-ok",
        "TIMEOUT": "pill pill-bad",
        "INCORRECT": "pill pill-warn",
        "FAIL": "pill pill-bad",
    }[kind]


def render_stats_card(label: str, value: str, detail: str) -> str:
    return (
        '<article class="stat-card">'
        f'<div class="stat-label">{html.escape(label)}</div>'
        f'<div class="stat-value">{html.escape(value)}</div>'
        f'<div class="stat-detail">{html.escape(detail)}</div>'
        "</article>"
    )


def render_html(
    *,
    subsystem: str,
    rows: list[dict[str, object]],
    report_path: Path,
    log_path: Path,
    selection_args: list[str],
    cpu_limit: str,
    generated_at: dt.datetime,
    command: str,
) -> str:
    defaults = DEFAULTS[subsystem]
    summary = summarize(rows)
    counts = summary["counts"]
    pass_total = summary["pass_seconds_total"]
    pass_median = summary["pass_seconds_median"]
    fastest = summary["fastest"]
    slowest = summary["slowest"]
    pass_count = int(summary["pass_count"])
    css_href = os.path.relpath(CSS_PATH, report_path.parent)
    log_href = os.path.relpath(log_path, report_path.parent)

    table_rows = "\n".join(
        (
            "<tr>"
            f"<td><span class=\"mono\">{html.escape(str(row['benchmark']))}</span></td>"
            f"<td><span class=\"mono\">{html.escape(str(row['display_name']))}</span></td>"
            f"<td><span class=\"{status_class(str(row['status']))}\">{html.escape(str(row['status']))}</span></td>"
            f"<td class=\"mono\">{html.escape(str(row['display_value']))}</td>"
            "</tr>"
        )
        for row in rows
    )

    stat_cards = "\n".join(
        [
            render_stats_card("Benchmarks seen", str(summary["total"]), "CSV rows parsed from this run"),
            render_stats_card("Passing", str(counts["PASS"]), "Completed and returned an expected result"),
            render_stats_card("Timeouts", str(counts["TIMEOUT"]), "Killed by the configured CPU limit"),
            render_stats_card("Incorrect", str(counts["INCORRECT"]), "Completed but produced the wrong result"),
            render_stats_card("Other failures", str(counts["FAIL"]), "Crash, missing source, or other non-timeout failure"),
        ]
    )

    pass_details = [
        f"Total passing runtime {format_duration(float(pass_total))}" if pass_count else "No passing timings captured",
        f"Median passing runtime {format_duration(float(pass_median))}" if isinstance(pass_median, float) else "Median unavailable",
        f"Fastest pass {format_duration(float(fastest))}" if isinstance(fastest, float) else "Fastest unavailable",
        f"Slowest pass {format_duration(float(slowest))}" if isinstance(slowest, float) else "Slowest unavailable",
    ]

    pills = "\n".join(
        f'<span class="{pill_class(kind)}">{kind} {counts[kind]}</span>'
        for kind in ["PASS", "TIMEOUT", "INCORRECT", "FAIL"]
    )

    table_copy = str(defaults["table_copy"]).format(
        log_href=html.escape(log_href),
        log_name=html.escape(log_path.name),
    )

    return f"""<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Arete benchmark report: {html.escape(subsystem)}</title>
    <link rel="stylesheet" href="{html.escape(css_href)}">
  </head>
  <body>
    <main class="report-shell">
      <section class="hero">
        <div class="hero-kicker">Arete Benchmark Report</div>
        <h1 class="hero-title">{html.escape(str(defaults["title"]))}</h1>
        <p class="hero-subtitle">
          {html.escape(str(defaults["subtitle"]))}
        </p>
        <div class="pill-row">
          {pills}
        </div>
        <div class="meta-grid">
          <article class="meta-card">
            <div class="meta-label">Generated</div>
            <div class="meta-value">{html.escape(generated_at.strftime("%Y-%m-%d %H:%M:%S UTC"))}</div>
          </article>
          <article class="meta-card">
            <div class="meta-label">{html.escape(str(defaults["selection_label"]))}</div>
            <div class="meta-value"><code>{html.escape(" ".join(selection_args))}</code></div>
          </article>
          <article class="meta-card">
            <div class="meta-label">CPU limit</div>
            <div class="meta-value"><code>{html.escape(cpu_limit)}</code> {html.escape(str(defaults["cpu_detail"]))}</div>
          </article>
          <article class="meta-card">
            <div class="meta-label">Host</div>
            <div class="meta-value">{html.escape(platform.node() or "unknown")}<br>{html.escape(platform.platform())}</div>
          </article>
        </div>
      </section>

      <section class="section">
        <h2 class="section-title">Summary</h2>
        <p class="section-copy">
          Command run: <code>{html.escape(command)}</code>
        </p>
        <div class="stats-grid">
          {stat_cards}
        </div>
        <div class="footer-note">
          {html.escape(pass_details[0])}. {html.escape(pass_details[1])}. {html.escape(pass_details[2])}. {html.escape(pass_details[3])}.
        </div>
      </section>

      <section class="section">
        <h2 class="section-title">Benchmark Results</h2>
        <p class="section-copy">
          {table_copy}
        </p>
        <div class="table-wrap">
          <table class="report-table">
            <thead>
              <tr>
                <th>Benchmark</th>
                <th>Invocation</th>
                <th>Status</th>
                <th>Timing / result</th>
              </tr>
            </thead>
            <tbody>
              {table_rows}
            </tbody>
          </table>
        </div>
      </section>
    </main>
  </body>
</html>
"""


def run_r7rs(args: argparse.Namespace) -> tuple[str, str]:
    log_path = args.log.resolve()
    ensure_parent(log_path)
    log_path.write_text("", encoding="utf-8")
    cpu_limit = str(args.cpu_limit)
    bench_args = args.benchmarks or ["perf"]
    command = [
        str(ROOT / "utils/run-r7rs-benchmarks.sh"),
        *bench_args,
    ]
    env = os.environ.copy()
    env["CPU_LIMIT"] = cpu_limit
    env["RESULTS"] = str(log_path)
    process = subprocess.run(
        command,
        cwd=ROOT,
        env=env,
        text=True,
        capture_output=True,
        check=False,
    )
    sys.stdout.write(process.stdout)
    sys.stderr.write(process.stderr)
    if process.returncode != 0:
        raise SystemExit(process.returncode)
    return (log_path.read_text(encoding="utf-8"), shell_join(command))


def preexec_cpu_limit(cpu_limit: int):
    def apply_limits() -> None:
        resource.setrlimit(resource.RLIMIT_CPU, (cpu_limit, cpu_limit))
        try:
            resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
        except (ValueError, OSError):
            pass

    return apply_limits


def resolve_arete_workloads(names: list[str]) -> list[str]:
    requested = names or list(DEFAULTS["arete"]["selection_default"])
    resolved: list[str] = []
    for name in requested:
        canonical = ARETE_ALIASES.get(name, name)
        if canonical == "all":
            resolved.extend(ARETE_WORKLOADS.keys())
            continue
        if canonical not in ARETE_WORKLOADS:
            raise SystemExit(f"unknown arete workload: {name}")
        resolved.append(canonical)
    deduped: list[str] = []
    seen: set[str] = set()
    for name in resolved:
        if name not in seen:
            seen.add(name)
            deduped.append(name)
    return deduped


def explicit_arete_selection(values: list[str] | None) -> list[str]:
    selected = split_selection_args(values)
    if not selected:
        return []
    return resolve_arete_workloads(selected)


def append_log(log_path: Path, line: str) -> None:
    with log_path.open("a", encoding="utf-8") as handle:
        handle.write(line)
        if not line.endswith("\n"):
            handle.write("\n")


def run_arete(args: argparse.Namespace) -> tuple[str, str]:
    log_path = args.log.resolve()
    ensure_parent(log_path)
    log_path.write_text("", encoding="utf-8")
    workloads = resolve_arete_workloads(args.workloads)
    cpu_limit = int(args.cpu_limit)
    runs = int(args.runs)
    arete_bin = ROOT / "bin/arete"
    if not arete_bin.exists():
        raise SystemExit(f"arete binary not found: {arete_bin} (run 'make bin/arete' first)")

    append_log(log_path, f"# arete workloads ({dt.datetime.now(dt.timezone.utc).isoformat()})")
    append_log(log_path, f"# arete={arete_bin} runs={runs} cpu_limit={cpu_limit}")

    command = [
        str(ROOT / "utils/benchmark-report.py"),
        "arete",
        *args.workloads,
        "--runs",
        str(runs),
        "--cpu-limit",
        str(cpu_limit),
    ]

    for workload_name in workloads:
        workload = ARETE_WORKLOADS[workload_name]
        append_log(log_path, "")
        append_log(log_path, f"# workload {workload_name}: {workload['description']}")
        timings: list[float] = []
        status: str | None = None

        for run_index in range(1, runs + 1):
            with tempfile.NamedTemporaryFile(prefix=f"arete-{workload_name}-", suffix=".log", delete=False) as handle:
                capture_path = Path(handle.name)
            start = time.perf_counter()
            try:
                with capture_path.open("w", encoding="utf-8") as capture:
                    process = subprocess.run(
                        list(workload["argv"]),
                        cwd=ROOT,
                        text=True,
                        stdout=capture,
                        stderr=subprocess.STDOUT,
                        check=False,
                        preexec_fn=preexec_cpu_limit(cpu_limit),
                    )
                elapsed = time.perf_counter() - start
                output = capture_path.read_text(encoding="utf-8", errors="replace")
            finally:
                capture_path.unlink(missing_ok=True)

            append_log(log_path, f"# run {run_index}/{runs}: {workload_name}")
            for line in output.splitlines():
                append_log(log_path, f"#   {line}")

            if process.returncode == 0:
                timings.append(elapsed)
                append_log(log_path, f"#   status=PASS elapsed={elapsed:.6f}s")
                continue
            if process.returncode in (124, 137, 152, -9, -24):
                status = "ULIMITKILLED"
            else:
                status = "CRASHED"
            append_log(log_path, f"#   status={status} returncode={process.returncode}")
            break

        if status is not None:
            append_log(log_path, f"+!CSVLINE!+{ARETE_SYSTEM},{workload_name},{status}")
            continue

        best = min(timings)
        append_log(log_path, f"+!CSVLINE!+{ARETE_SYSTEM},{workload_name},{best:.6f}")

    return (log_path.read_text(encoding="utf-8"), shell_join(command))


COMPARE_PERF_METRICS: tuple[tuple[str, tuple[str, ...]], ...] = (
    ("wall_time_us", ("wall_time_us",)),
    ("gc.collections", ("gc", "collections")),
    ("gc.allocations", ("gc", "allocations")),
    ("gc.collect_time_us", ("gc", "collect_time_us")),
    ("gc.time_in_gc_pct", ("gc", "time_in_gc_pct")),
    ("interpreter.calls", ("interpreter", "calls")),
    ("interpreter.exclusive_time_us", ("interpreter", "exclusive_time_us")),
    ("interpreter.exclusive_time_pct", ("interpreter", "exclusive_time_pct")),
    ("vm.calls", ("vm", "calls")),
    ("vm.exclusive_time_us", ("vm", "exclusive_time_us")),
    ("vm.exclusive_time_pct", ("vm", "exclusive_time_pct")),
    ("apply_calls", ("apply_calls",)),
    ("native_code_bytes", ("native_code_bytes",)),
)

COMPARE_NOISE_FLOOR_PCT = 5.0


def classify_returncode(returncode: int) -> str:
    if returncode == 0:
        return "PASS"
    if returncode in (124, 137, 152, -9, -24):
        return "TIMEOUT"
    return "FAIL"


def run_r7rs_once_for_compare(
    benchmark_args: list[str],
    cpu_limit: int,
    *,
    log_path: Path,
) -> tuple[list[dict[str, object]], str, str]:
    command = [
        str(ROOT / "utils/run-r7rs-benchmarks.sh"),
        *benchmark_args,
    ]
    env = os.environ.copy()
    env["CPU_LIMIT"] = str(cpu_limit)
    env["RESULTS"] = str(log_path)
    process = subprocess.run(
        command,
        cwd=ROOT,
        env=env,
        text=True,
        capture_output=True,
        check=False,
    )
    sys.stdout.write(process.stdout)
    sys.stderr.write(process.stderr)
    if process.returncode != 0:
        raise SystemExit(process.returncode)
    log_text = log_path.read_text(encoding="utf-8")
    rows = parse_log(log_text, R7RS_SYSTEM)
    return rows, shell_join(command), log_text


def run_arete_once_for_compare(
    workload_name: str,
    cpu_limit: int,
    *,
    capture_perf: bool,
) -> dict[str, object]:
    workload = ARETE_WORKLOADS[workload_name]
    with tempfile.NamedTemporaryFile(prefix=f"arete-{workload_name}-", suffix=".log", delete=False) as handle:
        capture_path = Path(handle.name)
    perf_path: Path | None = None
    if capture_perf:
        perf_path = capture_path.with_suffix(".perf.json")

    argv = [str(part) for part in workload["argv"]]
    if perf_path is not None:
        argv = [argv[0], "--perf-report", str(perf_path), *argv[1:]]

    start = time.perf_counter()
    try:
        with capture_path.open("w", encoding="utf-8") as capture:
            process = subprocess.run(
                argv,
                cwd=ROOT,
                text=True,
                stdout=capture,
                stderr=subprocess.STDOUT,
                check=False,
                preexec_fn=preexec_cpu_limit(cpu_limit),
            )
        elapsed = time.perf_counter() - start
        output = capture_path.read_text(encoding="utf-8", errors="replace")
    finally:
        capture_path.unlink(missing_ok=True)

    perf_report: dict[str, object] | None = None
    if perf_path is not None and perf_path.exists():
        perf_report = json.loads(perf_path.read_text(encoding="utf-8"))
        perf_path.unlink(missing_ok=True)

    status = classify_returncode(process.returncode)
    return {
        "workload": workload_name,
        "command": shell_join(argv),
        "status": status,
        "returncode": process.returncode,
        "seconds": elapsed if status == "PASS" else None,
        "milliseconds": (elapsed * 1000.0) if status == "PASS" else None,
        "output_tail": output.splitlines()[-20:],
        "perf_report": perf_report,
    }


def capture_compare_run(
    *,
    r7rs_selection: list[str],
    arete_selection: list[str],
    r7rs_cpu_limit: int,
    arete_cpu_limit: int,
    capture_perf_reports: bool,
    scratch_dir: Path,
) -> dict[str, object]:
    scratch_dir.mkdir(parents=True, exist_ok=True)
    generated_at = dt.datetime.now(dt.timezone.utc)
    cases: list[dict[str, object]] = []
    commands: list[str] = []
    artifacts: dict[str, object] = {}

    if r7rs_selection:
        r7rs_log_path = scratch_dir / "r7rs.log"
        rows, command, _log_text = run_r7rs_once_for_compare(
            r7rs_selection,
            r7rs_cpu_limit,
            log_path=r7rs_log_path,
        )
        commands.append(command)
        artifacts["r7rs_log"] = str(r7rs_log_path)
        for row in rows:
            cases.append(
                {
                    "subsystem": "r7rs",
                    "name": row["display_name"],
                    "benchmark": row["benchmark"],
                    "status": row["status"],
                    "raw_value": row["raw_value"],
                    "seconds": row["seconds"],
                    "milliseconds": row["milliseconds"],
                }
            )

    if arete_selection:
        arete_cases: list[dict[str, object]] = []
        for workload_name in arete_selection:
            sample = run_arete_once_for_compare(
                workload_name,
                arete_cpu_limit,
                capture_perf=capture_perf_reports,
            )
            arete_case = {
                "subsystem": "arete",
                "name": workload_name,
                "benchmark": workload_name,
                "status": sample["status"],
                "returncode": sample["returncode"],
                "seconds": sample["seconds"],
                "milliseconds": sample["milliseconds"],
                "output_tail": sample["output_tail"],
                "perf_report": sample["perf_report"],
            }
            commands.append(str(sample["command"]))
            arete_cases.append(arete_case)
            cases.append(arete_case)
        artifacts["arete_cases"] = [case["name"] for case in arete_cases]

    return {
        "type": "benchmark-capture",
        "generated_at": generated_at.isoformat(),
        "r7rs_selection": r7rs_selection,
        "arete_selection": arete_selection,
        "r7rs_cpu_limit": r7rs_cpu_limit,
        "arete_cpu_limit": arete_cpu_limit,
        "capture_perf_reports": capture_perf_reports,
        "commands": commands,
        "artifacts": artifacts,
        "cases": cases,
    }


def case_lookup(payload: dict[str, object]) -> dict[tuple[str, str], dict[str, object]]:
    cases = payload.get("cases")
    if not isinstance(cases, list):
        return {}
    result: dict[tuple[str, str], dict[str, object]] = {}
    for case in cases:
        if not isinstance(case, dict):
            continue
        subsystem = case.get("subsystem")
        name = case.get("name")
        if isinstance(subsystem, str) and isinstance(name, str):
            result[(subsystem, name)] = case
    return result


def compare_scalar_metric(current: object, baseline: object) -> dict[str, object] | None:
    if not isinstance(current, (int, float)) or not isinstance(baseline, (int, float)):
        return None
    current_value = float(current)
    baseline_value = float(baseline)
    delta = current_value - baseline_value
    delta_pct = None if baseline_value == 0 else (delta / baseline_value) * 100.0
    return {
        "baseline": baseline_value,
        "current": current_value,
        "delta": delta,
        "delta_pct": delta_pct,
    }


def compare_case(current_case: dict[str, object] | None, baseline_case: dict[str, object]) -> dict[str, object]:
    subsystem = str(baseline_case["subsystem"])
    name = str(baseline_case["name"])
    result = {
        "subsystem": subsystem,
        "name": name,
        "benchmark": baseline_case.get("benchmark", name),
        "baseline": baseline_case,
        "current": current_case,
    }

    if current_case is None:
        result["comparison"] = {
            "status": "MISSING",
            "timing": None,
            "perf_metrics": None,
        }
        return result

    baseline_status = str(baseline_case.get("status"))
    current_status = str(current_case.get("status"))
    timing = compare_scalar_metric(current_case.get("milliseconds"), baseline_case.get("milliseconds"))

    perf_metrics: dict[str, object] | None = None
    baseline_perf = baseline_case.get("perf_report")
    current_perf = current_case.get("perf_report")
    if isinstance(baseline_perf, dict) and isinstance(current_perf, dict):
        perf_metrics = {}
        for label, path in COMPARE_PERF_METRICS:
            metric = compare_scalar_metric(
                nested_get(current_perf, path),
                nested_get(baseline_perf, path),
            )
            if metric is not None:
                perf_metrics[label] = metric

    status = "UNCHANGED"
    if baseline_status != "PASS" or current_status != "PASS":
        if baseline_status == current_status:
            status = "STATUS_SAME"
        elif baseline_status == "PASS":
            status = "REGRESSION"
        else:
            status = "STATUS_CHANGED"
    elif timing is not None:
        delta_pct = timing["delta_pct"]
        if isinstance(delta_pct, float):
            if abs(delta_pct) < COMPARE_NOISE_FLOOR_PCT:
                status = "WITHIN_NOISE"
            elif delta_pct < 0:
                status = "IMPROVED"
            elif delta_pct > 0:
                status = "REGRESSED"

    result["comparison"] = {
        "status": status,
        "timing": timing,
        "perf_metrics": perf_metrics,
    }
    return result


def geomean_ratio(case_comparisons: list[dict[str, object]]) -> float | None:
    ratios: list[float] = []
    for case in case_comparisons:
        comparison = case.get("comparison")
        if not isinstance(comparison, dict):
            continue
        timing = comparison.get("timing")
        if not isinstance(timing, dict):
            continue
        baseline = timing.get("baseline")
        current = timing.get("current")
        if isinstance(baseline, (int, float)) and isinstance(current, (int, float)) and baseline > 0 and current > 0:
            ratios.append(float(current) / float(baseline))
    if not ratios:
        return None
    return math.exp(sum(math.log(ratio) for ratio in ratios) / len(ratios))


def print_capture_summary(payload: dict[str, object], baseline_path: Path) -> None:
    print(f"wrote baseline: {baseline_path}")
    print(f"+!BASELINE!+{baseline_path}")
    for case in payload["cases"]:
        status = str(case["status"])
        value = format_duration(float(case["seconds"])) if isinstance(case.get("seconds"), float) else str(case.get("raw_value", status))
        print(f"{case['subsystem']} {case['name']}: {status} {value}")


def print_compare_summary(payload: dict[str, object], output_path: Path) -> None:
    summary = payload["summary"]
    status = str(summary["status"])
    geomean = summary["geomean_ratio"]
    geomean_text = f"{geomean:.6f}" if isinstance(geomean, float) else "n/a"
    print(f"wrote comparison: {output_path}")
    print(f"+!COMPARE_STATUS!+{status}")
    print(f"+!COMPARE_GEOMEAN!+{geomean_text}")
    summary_bits = [
        f"comparable={summary['comparable_cases']}",
        f"within_noise={summary['within_noise_cases']}",
        f"improved={summary['improved_cases']}",
        f"regressed={summary['regressed_cases']}",
        f"status_failures={summary['status_failures']}",
    ]
    if isinstance(geomean, float):
        summary_bits.append(f"delta={format_pct_change((geomean - 1.0) * 100.0)}")
    summary_bits.append(f"noise_floor={summary['noise_floor_pct']:.1f}%")
    print("summary: " + " ".join(summary_bits))

    for case in payload["cases"]:
        comparison = case["comparison"]
        case_status = str(comparison["status"])
        timing = comparison["timing"]
        if not isinstance(timing, dict):
            baseline_status = case["baseline"]["status"]
            current_status = case["current"]["status"] if isinstance(case["current"], dict) else "MISSING"
            print(f"{case['subsystem']} {case['name']}: {case_status} {current_status} vs {baseline_status}")
            continue

        current_ms = float(timing["current"])
        baseline_ms = float(timing["baseline"])
        delta_ms = float(timing["delta"])
        delta_pct = timing["delta_pct"]
        print(
            f"{case['subsystem']} {case['name']}: {case_status} "
            f"{format_duration(current_ms / 1000.0)} vs {format_duration(baseline_ms / 1000.0)} "
            f"({delta_ms:+.3f}ms, {format_pct_change(delta_pct)})"
        )


def build_compare_payload(args: argparse.Namespace) -> dict[str, object]:
    baseline_path = args.baseline.resolve()
    if args.save_baseline:
        r7rs_selection = split_selection_args(args.r7rs)
        arete_selection = explicit_arete_selection(args.arete)
        if not r7rs_selection and not arete_selection:
            raise SystemExit("saving a baseline requires at least one of --r7rs or --arete")

        capture_payload = capture_compare_run(
            r7rs_selection=r7rs_selection,
            arete_selection=arete_selection,
            r7rs_cpu_limit=int(args.r7rs_cpu_limit or 60),
            arete_cpu_limit=int(args.arete_cpu_limit or 300),
            capture_perf_reports=not args.no_perf,
            scratch_dir=baseline_path.parent / f"{baseline_path.stem}.baseline-artifacts",
        )
        capture_payload["type"] = "benchmark-baseline"
        ensure_parent(baseline_path)
        baseline_path.write_text(json.dumps(capture_payload, indent=2) + "\n", encoding="utf-8")
        print_capture_summary(capture_payload, baseline_path)
        return capture_payload

    baseline_payload = json.loads(baseline_path.read_text(encoding="utf-8"))
    r7rs_selection = split_selection_args(args.r7rs) or list(baseline_payload.get("r7rs_selection", []))
    if split_selection_args(args.arete):
        arete_selection = explicit_arete_selection(args.arete)
    else:
        arete_selection = resolve_arete_workloads(list(baseline_payload.get("arete_selection", []))) if baseline_payload.get("arete_selection") else []
    capture_perf_reports = (
        False if args.no_perf else bool(baseline_payload.get("capture_perf_reports", True))
    )
    r7rs_cpu_limit = int(args.r7rs_cpu_limit or baseline_payload.get("r7rs_cpu_limit", 60))
    arete_cpu_limit = int(args.arete_cpu_limit or baseline_payload.get("arete_cpu_limit", 300))
    output_path = args.output.resolve() if args.output else baseline_path.with_name(f"{baseline_path.stem}.compare.json")

    current_payload = capture_compare_run(
        r7rs_selection=r7rs_selection,
        arete_selection=arete_selection,
        r7rs_cpu_limit=r7rs_cpu_limit,
        arete_cpu_limit=arete_cpu_limit,
        capture_perf_reports=capture_perf_reports,
        scratch_dir=output_path.parent / f"{output_path.stem}.artifacts",
    )

    baseline_cases = case_lookup(baseline_payload)
    current_cases = case_lookup(current_payload)
    comparisons = [
        compare_case(current_cases.get(key), baseline_case)
        for key, baseline_case in sorted(baseline_cases.items())
    ]
    comparable_cases = sum(
        1
        for case in comparisons
        if isinstance(case["comparison"].get("timing"), dict)
    )
    within_noise_cases = sum(
        1 for case in comparisons if case["comparison"]["status"] == "WITHIN_NOISE"
    )
    improved_cases = sum(1 for case in comparisons if case["comparison"]["status"] == "IMPROVED")
    regressed_cases = sum(
        1
        for case in comparisons
        if case["comparison"]["status"] in ("REGRESSED", "REGRESSION", "MISSING")
    )
    status_failures = sum(
        1
        for case in comparisons
        if case["comparison"]["status"] in ("REGRESSION", "MISSING")
    )
    ratio = geomean_ratio(comparisons)
    status = "FAIL" if status_failures else "PASS"

    payload = {
        "type": "benchmark-comparison",
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(),
        "baseline": str(baseline_path),
        "baseline_generated_at": baseline_payload.get("generated_at"),
        "baseline_capture": baseline_payload,
        "current_capture": current_payload,
        "summary": {
            "status": status,
            "comparable_cases": comparable_cases,
            "within_noise_cases": within_noise_cases,
            "improved_cases": improved_cases,
            "regressed_cases": regressed_cases,
            "status_failures": status_failures,
            "geomean_ratio": ratio,
            "geomean_delta_pct": ((ratio - 1.0) * 100.0) if isinstance(ratio, float) else None,
            "noise_floor_pct": COMPARE_NOISE_FLOOR_PCT,
        },
        "cases": comparisons,
    }

    ensure_parent(output_path)
    output_path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    print_compare_summary(payload, output_path)
    return payload


def split_selection_args(values: list[str] | None) -> list[str]:
    if not values:
        return []
    parts: list[str] = []
    for value in values:
        for part in value.split(","):
            stripped = part.strip()
            if stripped:
                parts.append(stripped)
    return parts


def write_report_bundle(
    *,
    subsystem: str,
    rows: list[dict[str, object]],
    report_path: Path,
    log_path: Path,
    selection_args: list[str],
    cpu_limit: str,
    generated_at: dt.datetime,
    command: str,
    extra: dict[str, object] | None = None,
) -> dict[str, object]:
    html_text = render_html(
        subsystem=subsystem,
        rows=rows,
        report_path=report_path,
        log_path=log_path,
        selection_args=selection_args,
        cpu_limit=cpu_limit,
        generated_at=generated_at,
        command=command,
    )
    report_path.write_text(html_text, encoding="utf-8")
    payload = build_payload(
        subsystem=subsystem,
        rows=rows,
        report_path=report_path,
        log_path=log_path,
        selection_args=selection_args,
        cpu_limit=cpu_limit,
        generated_at=generated_at,
        command=command,
        extra=extra,
    )
    metadata_path = report_path.with_suffix(".json")
    metadata_path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    print(f"wrote report: {report_path}")
    print(f"wrote metadata: {metadata_path}")
    return payload


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Run benchmarks and render HTML reports.")
    subparsers = parser.add_subparsers(dest="subcommand", required=True)

    r7rs = subparsers.add_parser("r7rs", help="Run the R7RS benchmark harness.")
    r7rs.add_argument("benchmarks", nargs="*", help=str(DEFAULTS["r7rs"]["selection_help"]))
    r7rs.add_argument("--cpu-limit", type=int, default=int(os.environ.get("CPU_LIMIT", "60")))
    r7rs.add_argument("--report", type=Path, default=DEFAULTS["r7rs"]["report"])
    r7rs.add_argument("--log", type=Path, default=DEFAULTS["r7rs"]["log"])
    r7rs.add_argument(
        "--from-log",
        type=Path,
        help="Skip running benchmarks and render from an existing log file instead.",
    )

    arete = subparsers.add_parser("arete", help="Run project-native Arete workloads.")
    arete.add_argument("workloads", nargs="*", help=str(DEFAULTS["arete"]["selection_help"]))
    arete.add_argument("--runs", type=int, default=int(os.environ.get("RUNS", "3")))
    arete.add_argument("--cpu-limit", type=int, default=int(os.environ.get("CPU_LIMIT", "300")))
    arete.add_argument("--report", type=Path, default=DEFAULTS["arete"]["report"])
    arete.add_argument("--log", type=Path, default=DEFAULTS["arete"]["log"])
    arete.add_argument(
        "--from-log",
        type=Path,
        help="Skip running workloads and render from an existing log file instead.",
    )

    series = subparsers.add_parser(
        "series",
        help="Run selected R7RS and Arete-native benchmarks into one output directory.",
    )
    series.add_argument(
        "--output-dir",
        type=Path,
        required=True,
        help="Directory where per-suite logs, HTML reports, and JSON results are written.",
    )
    series.add_argument(
        "--r7rs",
        action="append",
        default=[],
        metavar="NAME",
        help="R7RS benchmark or group to include. Repeat or use comma-separated values.",
    )
    series.add_argument(
        "--arete",
        action="append",
        default=[],
        metavar="NAME",
        help="Arete workload to include. Repeat or use comma-separated values.",
    )
    series.add_argument("--r7rs-cpu-limit", type=int, default=int(os.environ.get("CPU_LIMIT", "60")))
    series.add_argument("--arete-cpu-limit", type=int, default=int(os.environ.get("CPU_LIMIT", "300")))
    series.add_argument("--runs", type=int, default=int(os.environ.get("RUNS", "3")))

    compare = subparsers.add_parser(
        "compare",
        help="Save a benchmark baseline or compare the current run against one.",
    )
    compare.add_argument(
        "--baseline",
        type=Path,
        required=True,
        help="Path to the baseline JSON file to write or compare against.",
    )
    compare.add_argument(
        "--save-baseline",
        action="store_true",
        help="Capture one run and save it as the baseline instead of comparing.",
    )
    compare.add_argument(
        "--output",
        type=Path,
        help="Where to write the comparison JSON. Defaults next to the baseline file.",
    )
    compare.add_argument(
        "--r7rs",
        action="append",
        default=[],
        metavar="NAME",
        help="R7RS benchmark or group. On compare, defaults to the baseline selection.",
    )
    compare.add_argument(
        "--arete",
        action="append",
        default=[],
        metavar="NAME",
        help="Arete workload. On compare, defaults to the baseline selection.",
    )
    compare.add_argument(
        "--r7rs-cpu-limit",
        type=int,
        default=None,
        help="Override the R7RS CPU limit. Defaults to the baseline value when comparing.",
    )
    compare.add_argument(
        "--arete-cpu-limit",
        type=int,
        default=None,
        help="Override the Arete workload CPU limit. Defaults to the baseline value when comparing.",
    )
    compare.add_argument(
        "--no-perf",
        action="store_true",
        help="Do not collect --perf-report data for Arete workloads.",
    )

    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()

    if args.subcommand == "series":
        output_dir = args.output_dir.resolve()
        output_dir.mkdir(parents=True, exist_ok=True)
        generated_at = dt.datetime.now(dt.timezone.utc)
        manifest: dict[str, object] = {
            "subsystem": "series",
            "generated_at": generated_at.isoformat(),
            "output_dir": str(output_dir),
            "r7rs_selection": split_selection_args(args.r7rs),
            "arete_selection": split_selection_args(args.arete),
            "r7rs_cpu_limit": args.r7rs_cpu_limit,
            "arete_cpu_limit": args.arete_cpu_limit,
            "runs": args.runs,
            "artifacts": {},
        }

        if not manifest["r7rs_selection"] and not manifest["arete_selection"]:
            parser.error("series requires at least one of --r7rs or --arete")

        if manifest["r7rs_selection"]:
            r7rs_args = argparse.Namespace(
                subcommand="r7rs",
                benchmarks=list(manifest["r7rs_selection"]),
                cpu_limit=args.r7rs_cpu_limit,
                report=output_dir / "r7rs.html",
                log=output_dir / "r7rs.log",
                from_log=None,
            )
            log_text, command = run_r7rs(r7rs_args)
            rows = parse_log(log_text, R7RS_SYSTEM)
            payload = write_report_bundle(
                subsystem="r7rs",
                rows=rows,
                report_path=r7rs_args.report.resolve(),
                log_path=r7rs_args.log.resolve(),
                selection_args=r7rs_args.benchmarks or list(DEFAULTS["r7rs"]["selection_default"]),
                cpu_limit=str(r7rs_args.cpu_limit),
                generated_at=generated_at,
                command=command,
            )
            manifest["artifacts"]["r7rs"] = {
                "report": payload["report"],
                "log": payload["log"],
                "json": str((output_dir / "r7rs.json").resolve()),
            }

        if manifest["arete_selection"]:
            arete_args = argparse.Namespace(
                subcommand="arete",
                workloads=list(manifest["arete_selection"]),
                runs=args.runs,
                cpu_limit=args.arete_cpu_limit,
                report=output_dir / "arete.html",
                log=output_dir / "arete.log",
                from_log=None,
            )
            log_text, command = run_arete(arete_args)
            rows = parse_log(log_text, ARETE_SYSTEM)
            payload = write_report_bundle(
                subsystem="arete",
                rows=rows,
                report_path=arete_args.report.resolve(),
                log_path=arete_args.log.resolve(),
                selection_args=arete_args.workloads or list(DEFAULTS["arete"]["selection_default"]),
                cpu_limit=str(arete_args.cpu_limit),
                generated_at=generated_at,
                command=command,
                extra={"runs": arete_args.runs},
            )
            manifest["artifacts"]["arete"] = {
                "report": payload["report"],
                "log": payload["log"],
                "json": str((output_dir / "arete.json").resolve()),
            }

        manifest_path = output_dir / "manifest.json"
        manifest_path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
        print(f"wrote manifest: {manifest_path}")
        return 0

    if args.subcommand == "compare":
        build_compare_payload(args)
        return 0

    report_path = args.report.resolve()
    ensure_parent(report_path)

    if args.from_log:
        log_path = args.from_log.resolve()
        log_text = log_path.read_text(encoding="utf-8")
        if args.subcommand == "r7rs":
            expected_system = R7RS_SYSTEM
            selection_args = args.benchmarks or list(DEFAULTS["r7rs"]["selection_default"])
            command = shell_join(["utils/run-r7rs-benchmarks.sh", *selection_args])
        elif args.subcommand == "arete":
            expected_system = ARETE_SYSTEM
            selection_args = args.workloads or list(DEFAULTS["arete"]["selection_default"])
            command = shell_join(["utils/benchmark-report.py", "arete", *selection_args])
        else:
            parser.error(f"unsupported subsystem {args.subcommand}")
    else:
        if args.subcommand == "r7rs":
            expected_system = R7RS_SYSTEM
            selection_args = args.benchmarks or list(DEFAULTS["r7rs"]["selection_default"])
            log_text, command = run_r7rs(args)
        elif args.subcommand == "arete":
            expected_system = ARETE_SYSTEM
            selection_args = args.workloads or list(DEFAULTS["arete"]["selection_default"])
            log_text, command = run_arete(args)
        else:
            parser.error(f"unsupported subsystem {args.subcommand}")
        log_path = args.log.resolve()

    rows = parse_log(log_text, expected_system)
    extra: dict[str, object] | None = None
    if args.subcommand == "arete":
        extra = {"runs": args.runs}
    write_report_bundle(
        subsystem=args.subcommand,
        rows=rows,
        report_path=report_path,
        log_path=log_path,
        selection_args=selection_args,
        cpu_limit=str(args.cpu_limit),
        generated_at=dt.datetime.now(dt.timezone.utc),
        command=command,
        extra=extra,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
