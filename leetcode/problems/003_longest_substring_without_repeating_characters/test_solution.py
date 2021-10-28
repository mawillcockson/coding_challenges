"""
tests the solution in the named file
"""
import sys
from argparse import ArgumentParser
from importlib import import_module
from itertools import chain, starmap
from pathlib import Path
from pprint import pformat
from random import choices, randint, sample
from string import ascii_letters, digits, punctuation
from typing import Callable, NamedTuple, Optional

Function = Callable[[str], int]


class Parameters(NamedTuple):
    "parameters passed as star-args to the function under test"
    s: str


class TestCase(NamedTuple):
    "a test case"
    case: Parameters
    correct_answer: int


TEST_CASES = [
    ("abcabcbb", 3),
    ("bbbbb", 1),
    ("pwwkew", 3),
    ("", 0),
]
LONGEST_STRING = 50_000
ALL_CHARACTERS = list(set(ascii_letters + digits + punctuation))


def make_test_case(
    s: Optional[str] = None, correct_answer: Optional[int] = None
) -> TestCase:
    """
    generate a random test case, or make one from inputs
    """
    # ^ is bitwise XOR, and works with booleans
    if (s is None) ^ (correct_answer is None):
        raise ValueError(
            "either both 's' and 'correct_answer' need to be given, or neither"
        )
    elif s is not None and correct_answer is not None:
        return TestCase(Parameters(s=s), correct_answer=correct_answer)

    # The substring is guaranteed to be the longest unique substring if it's both
    # unique, and the longest.
    substring_length = randint(0, len(ALL_CHARACTERS))
    substring = sample(ALL_CHARACTERS, k=substring_length)
    rest_of_string_length = randint(0, min(LONGEST_STRING, substring_length - 1))
    rest_of_characters = choices(ALL_CHARACTERS, k=rest_of_string_length)
    insertion_index = randint(0, rest_of_string_length - 1)
    whole_string = [
        *rest_of_characters[:insertion_index],
        *substring,
        *rest_of_characters[insertion_index:],
    ]

    case = Parameters(s="".join(whole_string))
    return TestCase(case=case, correct_answer=substring_length)


def test(function: Function) -> None:
    "performs tests on the function to simulate the LeetCode submission"
    for case_index, test_case in enumerate(
        chain(
            starmap(make_test_case, TEST_CASES),
            (make_test_case() for _ in range(10_000)),
        )
    ):
        case_number = (
            str(case_index + 1) if case_index + 1 <= len(TEST_CASES) else "random"
        )
        if case_number != "random":
            print(f"test #{case_number}")

        case = test_case.case
        correct_answer = test_case.correct_answer

        answer = function(*case)
        if not answer or answer != correct_answer:
            print(f"failure for case #{case_number}:")
            print(f"case:\n{pformat(case)}")
            print(f"correct answer:\n{correct_answer}")
            print(f"answer:\n{answer}")
            # breakpoint()  # pylint: disable=forgotten-debug-statement
            sys.exit(1)

    print("passed")


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("file", help="filename of python file to test")
    args = parser.parse_args()
    path = Path(args.file)  # type: ignore
    if not path.is_file():
        print(f"{path} is not a file")
        sys.exit(1)

    sys.path.append(str(path.parent))
    module = import_module(path.stem)
    test(module.Solution().lengthOfLongestSubstring)  # type: ignore
