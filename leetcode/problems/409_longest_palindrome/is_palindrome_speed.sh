#!/bin/sh
set -eu
SETUP="
from test_solution import is_palindrome, is_palindrome2

true_cases = ['abcba', 'abba', 'aba', 'aa', 'a', '']
false_cases = ['abcb', 'abb', 'ab']
"
python -m timeit -s "${SETUP}" \
    "for case in true_cases:
    assert is_palindrome(case)
for case in false_cases:
    assert not is_palindrome(case)"
python -m timeit -s "${SETUP}" \
    "for case in true_cases:
    assert is_palindrome2(case)
for case in false_cases:
    assert not is_palindrome2(case)"
