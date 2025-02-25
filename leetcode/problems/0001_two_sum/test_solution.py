"""
tests the solution in the named file
"""
import os
import sys
from argparse import ArgumentParser
from importlib import import_module
from itertools import chain
from pathlib import Path
from pprint import pprint
from random import randint, shuffle
from typing import List, Tuple, TypedDict


class Parameters(TypedDict):
    nums: List[int]
    target: int


CorrectAnswer = Tuple[int, int]

TEST_CASES: List[Tuple[CorrectAnswer, Parameters]] = [
    ((6, 4), {"nums": [78, 72, 77, 95, -72, 96, 54], "target": -18})
]
LIMIT = 109


def random_test_case():
    # visual
    # https://www.desmos.com/calculator/mowk6elxin
    target = randint(-LIMIT, LIMIT)
    x_lower_bound = max(-LIMIT, target - LIMIT)
    x_upper_bound = min(LIMIT, target + LIMIT)
    x = randint(x_lower_bound, x_upper_bound)
    y = target - x

    array: List[int] = []
    length = randint(0, 5)
    while len(array) < length:
        other = randint(-LIMIT, LIMIT)
        if other not in [x, y] and target - other not in array:
            array.append(other)

    array.append(x)
    array.append(y)
    shuffle(array)
    x_index = array.index(x)
    y_index = array.index(y)
    if y_index == x_index:
        y_index = array.index(y, y_index + 1)
    return ((x_index, y_index), Parameters(nums=array, target=target))


def test(function) -> bool:
    for case_index, (correct_answer, case) in enumerate(
        chain(TEST_CASES, (random_test_case() for _ in range(100_000)))
    ):
        answer = function(**case)
        if not answer or set(answer) != set(correct_answer):
            print(f"failure for case #{case_index + 1}:")
            print("case:")
            pprint(case)
            print("correct answer:")
            pprint(correct_answer)
            print("answer:")
            pprint(answer)
            sys.exit(1)

    print("passed")


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("file", help="filename of python file to test")
    args = parser.parse_args()
    path = Path(args.file)
    if not path.is_file():
        print(f"{path} is not a file")
        sys.exit(1)

    sys.path.append(str(path.parent))
    module = import_module(path.stem)
    test(module.Solution().twoSum)
