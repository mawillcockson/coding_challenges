r"""

Submitted:
https://leetcode.com/submissions/detail/341347557/?from=/explore/challenge/card/may-leetcoding-challenge/534/week-1-may-1st-may-7th/3322/

In a binary tree, the root node is at depth 0, and children of each depth k node are at depth k+1.

Two nodes of a binary tree are cousins if they have the same depth, but have different parents.

We are given the root of a binary tree with unique values, and the values x and y of two different nodes in the tree.

Return true if and only if the nodes corresponding to the values x and y are cousins.

 

Example 1:
    1
   / \
  2   3
 /
4

Input: root = [1,2,3,4], x = 4, y = 3
Output: false

Example 2:

     1
    / \
   2   3
    \   \
     4   5

Input: root = [1,2,3,null,4,null,5], x = 5, y = 4
Output: true

Example 3:

    1
   / \ 
  2   3
   \
    4

Input: root = [1,2,3,null,4], x = 2, y = 3
Output: false

 

Constraints:

    The number of nodes in the tree will be between 2 and 100.
    Each node has a unique integer value from 1 to 100.
"""
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
from typing import Iterable, Optional
from collections import deque


# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right


class Node(TreeNode):
    def __init__(
        self,
        parent: Optional["Node"] = None,
        depth: int = 0,
        val: int = 0,
        left: Optional[TreeNode] = None,
        right: Optional[TreeNode] = None,
    ) -> None:
        self._val = val
        self._depth = depth
        self._parent = parent
        self._left = left
        self._right = right

    @property
    def val(self) -> int:
        return self._val

    @property
    def depth(self) -> int:
        return self._depth

    @property
    def parent(self) -> Optional["Node"]:
        return self._parent

    @property
    def left(self) -> Optional["Node"]:
        if self._left:
            return Node(
                val=self._left.val,
                depth=self._depth + 1,
                parent=self,
                left=self._left.left,
                right=self._left.right,
            )
        else:
            return None

    @property
    def right(self) -> Optional["Node"]:
        if self._right:
            return Node(
                val=self._right.val,
                depth=self._depth + 1,
                parent=self,
                left=self._right.left,
                right=self._right.right,
            )
        else:
            return None


class Solution:
    tree = dict()

    def nodes(self, root: TreeNode) -> Iterable[Node]:
        root_node = Node(
            val=root.val, parent=None, depth=0, right=root.right, left=root.left
        )
        queue = deque([root_node])
        while queue:
            head = queue.pop()
            left = head.left
            right = head.right
            if left:
                queue.append(left)
            if right:
                queue.append(right)
            yield head

    def isCousins(self, root: TreeNode, x: int, y: int) -> bool:
        x_node: Optional[Node] = None
        y_node: Optional[Node] = None
        for node in self.nodes(root):
            if x == node.val:
                x_node = node
            if y == node.val:
                y_node = node
            if x_node and y_node:
                break

        if not (x_node and y_node):
            return False
        return x_node.depth == y_node.depth and x_node.parent != y_node.parent
