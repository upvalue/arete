#!/usr/bin/env python3
"""Run the existing reader/preboot snapshots against bin/arete-rs."""

import glob
import os
import re
import subprocess
import sys

ansi_escape = re.compile(r"\x1b[^m]*m")


def arete_line(line):
    stripped = ansi_escape.sub("", line)
    return not stripped.startswith("arete:") and not stripped.startswith(";;")


def run_suite(exe, suite, args):
    print(f"running rust {suite} tests: ", end="")
    successful = 0
    total = 0

    test_paths = glob.glob(f"tests/{suite}/*.scm") + glob.glob(f"tests/{suite}/*.sld")
    for test_path in test_paths:
        result_path = test_path[:-3] + "exp"
        expect_error = False
        if not os.path.exists(result_path):
            if os.path.exists(test_path[:-3] + "err"):
                expect_error = True
            else:
                continue

        total += 1
        mapped_args = [arg.format(test_path) for arg in args]
        cmd = subprocess.Popen([exe] + mapped_args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = cmd.communicate()

        if expect_error:
            if cmd.returncode == 0:
                print(f"\n-- {test_path} was supposed to error but didn't:\n{stderr.decode().strip()}\n!")
                continue
        elif cmd.returncode != 0:
            print(f"\n-- {test_path} errored with text:\n{stderr.decode().strip()}\n!")
            continue
        else:
            result = stdout.decode("utf-8").strip()
            with open(result_path, "r", encoding="utf-8") as f:
                expected = f.read().strip()
            result = "\n".join(filter(arete_line, result.split("\n")))
            if expected != result:
                print(f"\n-- {test_path} failed, expected\n{expected}\n-- but got\n{result}\n!")
                continue

        successful += 1
        print("+", end="")

    print("")
    return successful, total


def main():
    exe = os.environ.get("ARETE_RS", "./bin/arete-rs")
    suites = {
        "reader": ["--read", "{}"],
        "preboot": ["{}"],
    }
    requested = sys.argv[1:] or list(suites)

    successful = 0
    total = 0
    for suite in requested:
        if suite not in suites:
            print(f"unknown suite: {suite}", file=sys.stderr)
            return 2
        s, t = run_suite(exe, suite, suites[suite])
        successful += s
        total += t

    print(f"{successful} out of {total} rust tests successful")
    return 0 if successful == total else 1


if __name__ == "__main__":
    raise SystemExit(main())
