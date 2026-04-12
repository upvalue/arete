#!/usr/bin/env python3

"""Run benchmark subsystems and render static HTML reports."""

from __future__ import annotations

import argparse
import datetime as dt
import html
import json
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
