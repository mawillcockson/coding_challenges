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
from typing import Callable, List, NamedTuple, Optional


class Parameters(NamedTuple):
    "parameters passed as star-args to the function under test"
    s: str


class Answer(NamedTuple):
    """
    the temporary output of the function under test to make it easier to assess
    answers
    """

    substring: str
    length: int


class TestCase(NamedTuple):
    "a test case"
    case: Parameters
    correct_answer: Answer


Function = Callable[[str], Answer]

TEST_CASES = [
    ("abcabcbb", "abc"),
    ("bbbbb", "b"),
    ("pwwkew", "wke"),
    ("", ""),
]
LONGEST_STRING = 50_000
ALL_CHARACTERS = list(set(ascii_letters + digits + punctuation))


def make_test_case(
    s: Optional[str] = None, correct_answer: Optional[str] = None
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
        return TestCase(
            Parameters(s=s),
            correct_answer=Answer(substring=correct_answer, length=len(correct_answer)),
        )

    # The substring is guaranteed to be the longest unique substring if it's
    # the longest substring within a string of characters all pulled from that
    # substring

    # draw a random sample of unique characters from a pool
    substring_length = randint(0, len(ALL_CHARACTERS))
    substring = sample(ALL_CHARACTERS, k=substring_length)

    # this substring will be placed within a longer string, and in order for it
    # to be the longest, it must be longer than both the substring in front of
    # it, and the substring after it
    pre_substring_length = randint(0, substring_length - 1)
    post_substring_length = randint(0, substring_length - 1)

    # those substrings can take characters from anywhere, since even if they're
    # unique, they won't be the longest
    pre_substring = choices(ALL_CHARACTERS, k=pre_substring_length)
    post_substring = choices(ALL_CHARACTERS, k=post_substring_length)

    # then, add characters from that substring until
    whole_string: List[str] = []
    whole_string.extend(pre_substring)
    whole_string.extend(substring)
    whole_string.extend(post_substring)

    case = Parameters(s="".join(whole_string))
    correct_answer = Answer(substring="".join(substring), length=substring_length)
    return TestCase(case=case, correct_answer=correct_answer)


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
