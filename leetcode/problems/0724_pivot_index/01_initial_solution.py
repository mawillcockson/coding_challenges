"""
https://leetcode.com/problems/find-pivot-index/?envType=study-plan&id=level-1
"""
import unittest


class Solution:
    "required by leetcode"

    def pivotIndex(self, nums: list[int]) -> int:
        """
        return the index of the point at which the left and right sums are
        equal
        """
        right = sum(nums)
        left = 0
        for i, num in enumerate(nums):
            right -= num
            if left == right:
                return i
            left += num

        return -1


class TestCases(unittest.TestCase):
    def setUp(self) -> None:
        self.func = Solution().pivotIndex

    def test_examples(self) -> None:
        examples = [
            ([1, 7, 3, 6, 5, 6], 3),
            ([1, 2, 3], -1),
            ([2, 1, -1], 0),
        ]

        for example, correct in examples:
            with self.subTest(example=example, correct=correct):
                out = self.func(example)
                self.assertEqual(out, correct)

    def test_additional(self) -> None:
        examples = [
            ([0, 0, 0, 0], 0),
            ([1, 0, 0, 1], 1),
            ([0, 1, 0, 1], 2),
            ([0, 1, 1, 0], -1),
        ]

        for example, correct in examples:
            with self.subTest(example=example, correct=correct):
                out = self.func(example)
