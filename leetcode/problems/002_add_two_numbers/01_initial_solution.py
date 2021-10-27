"""
You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order, and each of their nodes contains a single digit. Add the two numbers and return the sum as a linked list.

You may assume the two numbers do not contain any leading zero, except the number 0 itself.

Example 1:

Input: l1 = [2,4,3], l2 = [5,6,4]
Output: [7,0,8]
Explanation: 342 + 465 = 807.

Example 2:

Input: l1 = [0], l2 = [0]
Output: [0]

Example 3:

Input: l1 = [9,9,9,9,9,9,9], l2 = [9,9,9,9]
Output: [8,9,9,9,0,0,0,1]

Constraints:

- The number of nodes in each linked list is in the range [1, 100].
- 0 <= Node.val <= 9
- It is guaranteed that the list represents a number that does not have leading zeros.
"""
from itertools import zip_longest
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import Optional

    from test_solution import ListNode

if "ListNode" not in globals():
    try:
        from test_solution import ListNode
    except ImportError:

        class ListNode:
            "Definition for singly-linked list."

            def __init__(self, val: int = 0, next: "Optional[ListNode]" = None):
                self.val = val
                self.next = next


empty = ListNode()


class Solution:
    def addTwoNumbers(
        self, l1: "Optional[ListNode]", l2: "Optional[ListNode]"
    ) -> "Optional[ListNode]":
        l1 = l1 or empty
        l2 = l2 or empty
        start = ListNode()
        result = start

        # print(f"{l1.val} + {l2.val} //% 10 = ", end="")
        carry, result_digit = divmod(l1.val + l2.val, 10)
        # print(f"{carry}, {result_digit}")
        result.val = result_digit

        while l1.next or l2.next:
            result.next = ListNode()
            result = result.next

            l1 = l1.next or empty
            l2 = l2.next or empty

            digit1 = l1.val
            digit2 = l2.val
            # print(f"{digit1} + {digit2} + {carry} //% 10 = ", end="")
            carry, result_digit = divmod(digit1 + digit2 + carry, 10)
            # print(f"{carry}, {result_digit}")

            result.val = result_digit

        if carry:
            result.next = ListNode(carry, None)

        return start
