"""
tests the solution in the named file
"""
import sys
from argparse import ArgumentParser
from importlib import import_module
from itertools import chain, starmap
from pathlib import Path
from pprint import pformat
from random import randint
from typing import Callable, NamedTuple, Optional


class ListNode:
    "redefined and expanded from LeetCode"

    def __init__(self, val: int = 0, next: "Optional[ListNode]" = None):
        # pylint: disable=redefined-builtin
        self.val = val
        self.next = next

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, ListNode):
            return False

        current_self = self

        if not current_self.val == other.val:
            return False

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

    @classmethod
    def from_int(cls: "Type[ListNode]", number: int) -> "ListNode":
        "from https://stackoverflow.com/a/39644726/5059062"
        start = ListNode()
        result = start
        while number:
            result.val = number % 10
            number = number // 10
            if not number:
                break

            result.next = ListNode()
            result = result.next

        result.next = None
        return start


Function = Callable[[Optional[ListNode], Optional[ListNode]], Optional[ListNode]]


class Parameters(NamedTuple):
    "parameters passed as star-args to the function under test"
    l1: ListNode
    l2: ListNode


class TestCase(NamedTuple):
    "a test case"
    case: Parameters
    correct_answer: ListNode


TEST_CASES = [
    (9, 9),
    (1, 1),
    (342, 465),
    (0, 0),
    (9_999_999, 9_999),
]
LARGEST_NUMBER_100_DIGITS_LONG = 10 ** 100 - 1
LISTNODE_EQUALITIES = [(1, 1), (1, 2)]


def make_test_case(num1: Optional[int], num2: Optional[int]) -> TestCase:
    "make a test case using specific or random numbers"
    if num1 is None:
        num1 = randint(0, LARGEST_NUMBER_100_DIGITS_LONG)
    if num2 is None:
        num2 = randint(0, LARGEST_NUMBER_100_DIGITS_LONG)

    correct_answer = num1 + num2

    case = Parameters(l1=ListNode.from_int(num1), l2=ListNode.from_int(num2))

    return TestCase(case=case, correct_answer=ListNode.from_int(correct_answer))


def test(function: Function) -> None:
    "performs tests on the function to simulate the LeetCode submission"
    for num1, num2 in LISTNODE_EQUALITIES:
        if (ListNode.from_int(num1) == ListNode.from_int(num2)) != (num1 == num2):
            print("ListNode equality failure: {num1}, {num2}")
            sys.exit(1)

    for case_index, test_case in enumerate(
        chain(
            starmap(make_test_case, TEST_CASES),
            (make_test_case(num1=None, num2=None) for _ in range(100_000)),
        )
    ):
        case_number = str(case_index + 1) if case_index <= len(TEST_CASES) else "random"
        if case_number != "random":
            print(".")

        case = test_case.case
        correct_answer = test_case.correct_answer

        answer = function(*case)
        if not answer or answer != correct_answer:
            print(f"failure for case #{case_number}:")
            print(f"case:\n{pformat(case)}")
            print(f"correct answer:\n{correct_answer}")
            print(
                f"answer:\n{ListNode.__repr__(answer) if answer is not None else 'None'}"
            )
            breakpoint()  # pylint: disable=forgotten-debug-statement
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
    test(module.Solution().addTwoNumbers)  # type: ignore
