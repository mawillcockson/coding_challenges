"""
tests the solution in the named file
"""
import math
import sys
from argparse import ArgumentParser
from importlib import import_module
from itertools import chain, starmap
from pathlib import Path
from pprint import pformat
from random import randint, random
from statistics import median
from typing import Callable, List, NamedTuple, Tuple


class Parameters(NamedTuple):
    "parameters passed as star-args to the function under test"
    nums1: List[int]
    nums2: List[int]


Answer = float


class TestCase(NamedTuple):
    "a test case"
    case: Parameters
    correct_answer: Answer


Function = Callable[[List[int], List[int]], Answer]

TEST_CASES: List[Tuple[List[int], List[int], float]] = [
    ([0, 2, 3], [1], 1.5),
    ([0, 8], [1, 1, 9], 1.0),
    ([1, 3, 7, 9], [2, 5, 8], 5.0),
    ([0, 8], [1, 1, 7], 1.0),
    ([0, 0], [1, 1, 1], 1.0),
    ([0, 0, 0, 3], [1, 4], 0.5),
    ([1, 3], [2], 2.0),
    ([1, 2], [3, 4], 2.5),
    ([0, 0], [0, 0], 0.0),
    ([], [1], 1.0),
    ([2], [], 2.0),
    ([0], [1], 0.5),
    ([1], [0], 0.5),
]
NUM_RANDOM_TESTS = 10_000
MAX_LENGTH = 1_000
MAX_INT = 10 ** 6
MIN_INT = -MAX_INT
DEBUGGING = False


def make_test_case(
    nums1: List[int],
    nums2: List[int],
    correct_answer: Answer,
) -> TestCase:
    "make a test case from inputs"
    return TestCase(
        Parameters(nums1=nums1, nums2=nums2),
        correct_answer=correct_answer,
    )


def generate_test_case() -> TestCase:
    "generate a random test case"
    nums1_length = randint(0, MAX_LENGTH)
    if nums1_length == 0:
        nums2_length = randint(1, MAX_LENGTH)
    else:
        nums2_length = randint(0, MAX_LENGTH)

    # this won't be exactly correct
    nums1 = [int(random() * (MAX_INT - MIN_INT) - MIN_INT) for _ in range(nums1_length)]
    nums2 = [int(random() * (MAX_INT - MIN_INT) - MIN_INT) for _ in range(nums2_length)]

    # # more correct
    # nums1 = [randint(MIN_INT, MAX_INT) for _ in range(nums1_length)]
    # nums2 = [randint(MIN_INT, MAX_INT) for _ in range(nums2_length)]

    nums1.sort()
    nums2.sort()
    correct_answer = Answer(median([*nums1, *nums2]))

    if randint(0, 1):
        return TestCase(
            Parameters(nums1=nums1, nums2=nums2), correct_answer=correct_answer
        )
    return TestCase(Parameters(nums1=nums2, nums2=nums1), correct_answer=correct_answer)


def test(function: Function) -> None:
    "performs tests on the function to simulate the LeetCode submission"
    passed_count = 0
    for case_index, test_case in enumerate(
        chain(
            starmap(make_test_case, TEST_CASES),
            (generate_test_case() for _ in range(NUM_RANDOM_TESTS)),
        )
    ):
        random_case = case_index + 1 > len(TEST_CASES)
        if not random_case:
            case_number = str(case_index + 1)
            print(f"test #{case_number}")
        elif DEBUGGING:
            sys.exit("passed all explicit")
        else:
            case_number = "random"

        case = test_case.case
        correct_answer = test_case.correct_answer

        try:
            answer = function(*case)
            passed_count += 1
        except NotImplementedError:
            if not random_case:
                print("skipped")
            continue

        if not check(answer, correct_answer):
            print(f"failure for case #{case_number}:")
            print(f"case:\n{pformat(case)}")
            print(f"correct answer:\n{correct_answer}")
            print(f"answer:\n{answer}")
            # breakpoint()  # pylint: disable=forgotten-debug-statement
            sys.exit(1)

    print(f"passed {passed_count} out of {len(TEST_CASES) + NUM_RANDOM_TESTS}")


def check(answer: Answer, correct_answer: Answer) -> bool:
    "check if the answer is correct"
    return math.isclose(answer, correct_answer)


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
    test(module.Solution().findMedianSortedArrays)  # type: ignore
