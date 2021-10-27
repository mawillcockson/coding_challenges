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
from typing import List, Optional, Tuple, TypedDict


class ListNode:
    def __init__(self, val: int = 0, next: "Optional[ListNode]" = None):
        self.val = val
        self.next = next

    def __eq__(self, other: "ListNode") -> bool:
        current_self = self
        while current_self.next and other.next:
            if not current_self.val == other.val:
                return False

            current_self = current_self.next
            other = other.next

        # We only get here after one is exhausted, so if one still has another
        # one left, they're of unequal length, and not equal
        if current_self.next or other.next:
            return False

        return True

    def __repr__(self) -> str:
        string = ""
        current_self: "Optional[ListNode]" = self
        while current_self:
            string = str(current_self.val) + string
            current_self = current_self.next

        return f"<ListNode: {string}>"


class Parameters(TypedDict):
    l1: int
    l2: int


CorrectAnswer = int


class TestCase(TypedDict):
    case: Parameters
    correct_answer: CorrectAnswer


TEST_CASES: List[TestCase] = [
    {"case": {"l1": 342, "l2": 465}, "correct_answer": 807},
    {"case": {"l1": 0, "l2": 0}, "correct_answer": 0},
    {"case": {"l1": 9_999_999, "l2": 9_999}, "correct_answer": 89_990_001},
]
LARGEST_NUMBER_100_DIGITS_LONG = 10 ** 100 - 1


def int_to_ListNode(number: int) -> ListNode:
    "from https://stackoverflow.com/a/39644726/5059062"
    start = ListNode()
    result = start
    while number:
        result.val = number % 10
        result.next = ListNode()
        result = result.next
        number = number // 10

    result.next = None
    return start


def random_test_case():
    num1 = randint(0, LARGEST_NUMBER_100_DIGITS_LONG)
    num2 = randint(0, LARGEST_NUMBER_100_DIGITS_LONG)
    correct_answer = num1 + num2
    return TestCase(case=case, correct_answer=correct_answer)


def test(function) -> bool:
    for case_index, test_case in enumerate(
        chain(TEST_CASES, (random_test_case() for _ in range(100_000)))
    ):
        case = test_case["case"]
        correct_answer = test_case["correct_answer"]

        l1 = int_to_ListNode(case["l1"])
        l2 = int_to_ListNode(case["l2"])
        correct_answer = int_to_ListNode(correct_answer)

        answer = function(l1=l1, l2=l2)
        if not answer or answer != correct_answer:
            print(f"failure for case #{case_index + 1}:")
            print("case:")
            pprint(case)
            print("correct answer:")
            pprint(correct_answer)
            print("answer:")
            if not answer is None:
                print(ListNode.__repr__(answer))
            else:
                print("None")
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
    test(module.Solution().addTwoNumbers)
