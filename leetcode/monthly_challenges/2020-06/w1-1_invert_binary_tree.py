# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right


# class TreeNode:
#     def __init__(
#         self,
#         val: int = 0,
#         left: Optional[TreeNode] = None,
#         right: Optional[TreeNode] = None,
#     ):
#         self.val = val
#         self.left = left
#         self.right = right


from collections import deque
from typing import Deque, Optional, Tuple


def children(node: TreeNode) -> Tuple[Optional[TreeNode], Optional[TreeNode]]:
    return (node.right, node.left)


class Solution:
    def invertTree(self, root: TreeNode) -> TreeNode:
        if not root:
            return root
        queue: Deque[TreeNode] = deque([root])
        while queue:
            node = queue.pop()
            right, left = children(node)
            node.left = right
            node.right = left
            if right:
                queue.append(right)
            if left:
                queue.append(left)

        return root

# Submitted: https://leetcode.com/submissions/detail/348819815/?from=/explore/challenge/card/june-leetcoding-challenge/539/week-1-june-1st-june-7th/3347/

# This was in the answers, and I can't believe I didn't have the guts to try
# for a recursive solution
# class Solution:
#     def invertTree(self, root: TreeNode) -> TreeNode:
#         if not root or (not root.left and not root.right): return root
#         
#         root.left, root.right = self.invertTree(root.right), self.invertTree(root.left)
#     
#         return root

