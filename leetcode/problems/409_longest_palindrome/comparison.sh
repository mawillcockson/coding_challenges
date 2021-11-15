#!/bin/sh
set -eu
SETUP="a = list(range(2_000))"
python -m timeit -s "${SETUP}" 'b = [*a, *a[::-1]]'
python -m timeit -s "${SETUP}" 'b = a + a[::-1]'
python -m timeit -s "${SETUP}" 'b = a[:];b.extend(a[::-1])'
python -m timeit -s "${SETUP}" 'b = a[:];b.extend(reversed(a))'
