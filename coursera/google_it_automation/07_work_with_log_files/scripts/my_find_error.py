#!/usr/bin/env python3
import re
import sys
from pathlib import Path
from typing import Final

LOG_FILE: Final = Path(__file__).parent.parent / "data" / "fishy.log"
OUTPUT_FILE: Final = LOG_FILE.parent / "errors_found.log"


def error_search(log_file: str) -> list[str]:
    user_patterns = input("Search patterns? > ")

    matchers = [re.compile(p) for p in ["error", ...(user_patterns.split(" "))]]

    lines: list[str] = []
    with open(log_file, mode="rt", encoding="utf-8") as f:
        for line in f:
            if all(matcher.search(line) for matcher in matchers):
                lines.append(line)

    return lines


def file_output(returned_errors: list[str]) -> None:
    joiner = ""
    if any((not line.endswith("\n")) for line in returned_errors):
        joiner = "\n"
    OUTPUT_FILE.write_text(joiner.join(returned_errors))


if __name__ == "__main__":
    log_file = str(LOG_FILE)
    if len(sys.argv) > 0:
        log_file = sys.argv[1]
    returned_errors = error_search(log_file)
    file_output(returned_errors)
    sys.exit(0)
