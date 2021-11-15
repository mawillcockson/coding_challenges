# pylint: disable=invalid-name
"""
tests the solution in the named file

https://leetcode.com/problems/longest-palindrome/

Given a string s which consists of lowercase or uppercase letters, return the
length of the longest palindrome that can be built with those letters.

Letters are case sensitive, for example, "Aa" is not considered a palindrome
here.

Example 1:
Input: s = "abccccdd"
Output: 7
Explanation:
One longest palindrome that can be built is "dccaccd", whose length is 7.

Example 2:
Input: s = "a"
Output: 1

Example 3:
Input: s = "bb"
Output: 2

Constraints:

- 1 <= s.length <= 2000
- s consists of lowercase and/or uppercase English letters only.
"""
import inspect
import string
import sys
from argparse import ArgumentParser
from copy import copy
from importlib import import_module
from itertools import chain, cycle, starmap
from pathlib import Path
from pprint import pformat
from random import choices, randint, randrange, shuffle
from typing import Callable, List, NamedTuple, Sequence, Tuple, TypeVar


class EmptyClass:  # pylint: disable=too-few-public-methods
    "empty class for attribute comparison"


class Parameters(NamedTuple):
    "parameters passed as star-args to the function under test"
    s: str


Answer = int


class TestCase(NamedTuple):
    "a test case"
    case: Parameters
    correct_answer: Answer


Function = Callable[[str], Answer]

TEST_CASES: List[Tuple[str, int]] = [
    ("abccccdd", 7),
    ("a", 1),
    ("bb", 2),
]
NUM_RANDOM_TESTS = 1_000_000
MAX_LENGTH = 2_000
LETTERS = string.ascii_letters
DEBUGGING = True
T = TypeVar("T")


def disable_debugging() -> None:
    "makes the identifier a global"


def make_test_case(
    s: str,
    correct_answer: Answer,
) -> TestCase:
    "make a test case from inputs"
    return TestCase(
        Parameters(s=s),
        correct_answer=correct_answer,
    )


def pick_random_generator(generators: List[Callable[[], TestCase]]) -> TestCase:
    "randomly picks a test case generator, and executes it"
    if len(generators) > 1:
        return generators[randrange(len(generators))]()

    return generators[0]()


def generate_test_case1() -> TestCase:
    "mirror a random string"
    # generate a random string, then mirror it
    # this guarantees that if there are any other correct answers, they're all
    # the same length: the length of the string
    total_length = randint(1, MAX_LENGTH)
    s = "".join(choices(LETTERS, k=total_length // 2))
    if total_length % 2 == 1:  # is odd
        s = s + s[-2::-1]  # all but the last element, reversed
    else:
        s = s + s[::-1]

    if not is_palindrome(s):
        raise Exception(f"{s} is not a palindrome")

    correct_answer = len(s)
    return TestCase(Parameters(s=s), correct_answer=correct_answer)


def generate_test_case2() -> TestCase:
    "construct a test case by palindrome properties"
    s: List[str] = []
    max_length = randint(1, MAX_LENGTH)
    correct_answer = randint(1, max_length)
    remaining_count = correct_answer // 2

    shuffled_letters = list(copy(LETTERS))
    shuffle(shuffled_letters)
    for letter in cycle(shuffled_letters):
        if remaining_count <= 0:
            break
        count = randint(0, remaining_count)
        s.extend(letter * count)
        remaining_count -= count

    s.extend(reversed(s))
    if not is_palindrome(s):
        raise Exception(f"{s} is not a palindrome")

    assert (
        len(s) == (correct_answer // 2) * 2
    ), f"generated string incorrectly: {len(s)} =/= {(correct_answer // 2) * 2}"

    if correct_answer % 2 == 1:
        unused_letters = list(set(LETTERS) - set(s))
        if unused_letters:
            num_extra = min(max_length - len(s), len(unused_letters))
            s.extend(unused_letters[:num_extra])
        else:
            correct_answer -= 1

    assert len(s) <= max_length, f"added too many letters: {len(s)} > {max_length}"

    shuffle(s)
    return TestCase(Parameters(s="".join(s)), correct_answer=correct_answer)


def test(function: Function) -> None:
    "performs tests on the function to simulate the LeetCode submission"
    globs = globals()  # type: ignore
    generators: List[Callable[[], TestCase]] = [
        globs[name]  # type: ignore
        for name in globs  # type: ignore
        if name != "generate_test_case"
        and name.startswith("generate_test_case")
        and inspect.isfunction(globs[name])  # type: ignore
    ]

    passed_count = 0
    for case_index, test_case in enumerate(
        chain(
            starmap(make_test_case, TEST_CASES),
            (pick_random_generator(generators) for _ in range(NUM_RANDOM_TESTS)),
        )
    ):
        random_case = case_index + 1 > len(TEST_CASES)
        if not random_case:
            case_number = str(case_index + 1)
            print(f"test #{case_number}")
        else:
            disable_debugging()
            global DEBUGGING
            DEBUGGING = False
            case_number = "random"

        case = test_case.case
        correct_answer = test_case.correct_answer

        try:
            answer = function(*case)
        except NotImplementedError:
            if not random_case:
                print("skipped")
            continue
        else:
            passed_count += 1

        if not check(answer, correct_answer):
            if not DEBUGGING:
                # run again so debugging statements are prints
                function(*case)
            print(f"failure for case #{case_number}:")
            print(f"case:\n{pformat(case)}")
            print(f"correct answer:\n{correct_answer}")
            print(f"answer:\n{answer}")
            # breakpoint()  # pylint: disable=forgotten-debug-statement
            sys.exit(1)

    print(f"passed {passed_count} out of {len(TEST_CASES) + NUM_RANDOM_TESTS}")


def check(answer: Answer, correct_answer: Answer) -> bool:
    "check if the answer is correct"
    return answer == correct_answer


def is_palindrome(sequence: Sequence[T]) -> bool:
    "determines if sequence is a palindrome"
    # print(repr(sequence))  # debug
    length = len(sequence)
    if length in [0, 1]:
        return True

    half = length // 2
    if length % 2 == 0:  # is odd
        # print(f"{sequence[:half]} =?= {sequence[: half - 1 : -1]}")  # debug
        return sequence[:half] == sequence[: half - 1 : -1]
    # print(f"{sequence[:half]} =?= {sequence[:half:-1]}")  # debug
    return sequence[:half] == sequence[:half:-1]


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
    DEBUGGING = bool(module.DEBUGGING)  # type: ignore
    solution = module.Solution()  # type: ignore
    extra_attributes = set(dir(solution)) - set(dir(EmptyClass()))  # type: ignore
    solution_methods = [
        name
        for name in extra_attributes
        if inspect.ismethod(getattr(solution, name)) and "_" not in name  # type: ignore
    ]

    if len(solution_methods) > 1:
        raise Exception(
            f"more than one solution method name found; pick one of {', '.join(solution_methods)}"
        )
    if len(solution_methods) < 1:
        raise Exception("no solution methods found")

    def turn_off_debugging() -> None:
        "sets the module's DEBUGGING to False"
        module.DEBUGGING = False  # type: ignore

    disable_debugging = turn_off_debugging

    test(getattr(solution, solution_methods[0]))  # type: ignore
