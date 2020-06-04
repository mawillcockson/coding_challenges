"""
Constraints:

- The linked list will have at least two elements.
- All of the nodes' values will be unique.
- The given node will not be the tail and it will always be a valid node of the linked list.
- Do not return anything from your function.
"""
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

from typing import Any, Optional

# class ListNode:
#     def __init__(self, x: Any):
#         self.val = x
#         self.next: Optional[ListNode] = None


class Solution:
    def deleteNode(self, node: ListNode) -> None:
        """
        :type node: ListNode
        :rtype: void Do not return anything, modify node in-place instead.
        """
        if not node.next:
            return None

        while node.next.next:
            node.val = node.next.val
            node = node.next

        # This is just to satisfy mypy
        if not node.next:
            return None

        node.val = node.next.val
        node.next = None


# Submitted: https://leetcode.com/submissions/detail/348828725/?from=/explore/challenge/card/june-leetcoding-challenge/539/week-1-june-1st-june-7th/3348/

# I feel so dumb:
# class Solution:
#     def deleteNode(self, node):
#         """
#         :type node: ListNode
#         :rtype: void Do not return anything, modify node in-place instead.
#         """
#         node.val = node.next.val
#         node.next = node.next.next

