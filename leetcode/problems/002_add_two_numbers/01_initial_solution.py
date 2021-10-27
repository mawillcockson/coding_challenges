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

Submitted: https://leetcode.com/submissions/detail/578163768/
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


class Solution:
    def addTwoNumbers(
        self, l1: "Optional[ListNode]", l2: "Optional[ListNode]"
    ) -> "Optional[ListNode]":
        """
        since the nodes start at the "1s place", adding each pair of digits
        from the start will add the two numbers together, just like I would do
        on paper with long addition

        this lets me carry over numbers very naturally, mimicking the algorithm
        even better
        """
        # An empty ListNode used both because 0 added to anything doesn't
        # change its value, and the it's a leaf or end node (i.e. it doesn't
        # point to a next node)
        empty = ListNode(0, None)

        # I'm not sure why any of the parameters are marked as optional, as
        # there's always a value when run by LeetCode, but because they are,
        # it's  to check if they're None necessary
        l1 = l1 or empty
        l2 = l2 or empty

        # Remember to first node in the sum's linked list
        first_node = ListNode()
        next_node = first_node

        # Do the first addition outside the loop, since the condition checks if
        # there's a next value, and if both lists passed in are only 1 digit
        # long, the loop will be skipped, skipping that first addition

        # divmod() does the equivalent of integer division and modulus:
        #
        # carry = (l1.val + l2.val) // 10
        # result_digit = (l1.val + l2.val) % 10
        #
        # print(f"{l1.val} + {l2.val} //% 10 = ", end="") # debug
        carry, result_digit = divmod(l1.val + l2.val, 10)
        # print(f"{carry}, {result_digit}") # debug
        next_node.val = result_digit

        # if either has a next node
        while l1.next or l2.next:
            # Add a node to the sum's list
            next_node.next = ListNode()
            next_node = next_node.next

            # get the next node for both input lists if there is one, and if
            # there isn't, use the empty node
            l1 = l1.next or empty
            l2 = l2.next or empty

            digit1 = l1.val
            digit2 = l2.val
            # print(f"{digit1} + {digit2} + {carry} //% 10 = ", end="")
            carry, result_digit = divmod(digit1 + digit2 + carry, 10)
            # print(f"{carry}, {result_digit}")

            next_node.val = result_digit

        # Once the loop is done, if there's a leftover remainder, add it as an end node to the sum
        if carry:
            next_node.next = ListNode(carry, None)

        # Return the first node
        return first_node
