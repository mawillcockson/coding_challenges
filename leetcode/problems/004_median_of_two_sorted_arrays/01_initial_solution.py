# pylint: disable=invalid-name
"""
Given two sorted arrays nums1 and nums2 of size m and n respectively, return
the median of the two sorted arrays.

The overall run time complexity should be O(log (m+n)).


Example 1:
Input: nums1 = [1,3]
       nums2 = [2]
Output: 2.00000
Explanation: merged array = [1,2,3] and median is 2.

Example 2:
Input: nums1 = [1,2]
       nums2 = [3,4]
Output: 2.50000
Explanation: merged array = [1,2,3,4] and median is (2 + 3) / 2 = 2.5.

Example 3:
Input: nums1 = [0,0]
       nums2 = [0,0]
Output: 0.00000

Example 4:
Input: nums1 = []
       nums2 = [1]
Output: 1.00000

Example 5:
Input: nums1 = [2]
       nums2 = []
Output: 2.00000


Constraints:

- nums1.length == m
- nums2.length == n
- 0 <= m <= 1000
- 0 <= n <= 1000
- 1 <= m + n <= 2000
- -10^6 <= nums1[i], nums2[i] <= 10^6
"""
from typing import List


def median_of_sorted(nums: List[int], length: int) -> float:
    "find the median of an already sorted list"
    if length == 1:
        return float(nums[0])

    if length % 2 == 0:  # is even
        return float(nums[length // 2])

    lower_median_index = length // 2
    return (nums[lower_median_index] + nums[lower_median_index + 1]) / 2


class Solution:
    # pylint: disable=no-self-use,too-few-public-methods
    "required by leetcode"

    def findMedianSortedArrays(self, nums1: List[int], nums2: List[int]) -> float:
        """
        find the median of an array made from the sorted combination of two arrays

        the two arrays are passed in pre-sorted
        """
        nums1_length = len(nums1)
        nums2_length = len(nums2)

        # shortcut cases
        if nums1_length == 0:
            return median_of_sorted(nums2, nums2_length)
        if nums2_length == 0:
            return median_of_sorted(nums1, nums1_length)
