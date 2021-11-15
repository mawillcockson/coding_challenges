#!/bin/sh
set -eu
SETUP="
def f1(value: bool) -> int:
    return 1 + value

def f2(value: bool) -> int:
    if value:
        return 1 + 1
    return 1
"
python -m timeit -s "${SETUP}" \
    "f1(True);f1(False)"
python -m timeit -s "${SETUP}" \
    "f2(True);f2(False)"
