# pylint: disable=invalid-name
"""
https://leetcode.com/problems/median-of-two-sorted-arrays/

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


try:
    import test_solution
except ImportError:
    DEBUGGING = False
else:
    DEBUGGING = True

DEBUGGING = False


def median_of_sorted(nums: List[int], length: int) -> float:
    "find the median of an already sorted list"
    if length == 1:
        return float(nums[0])

    if length % 2 == 1:  # is odd
        return float(nums[length // 2])

    upper_median_index = length // 2
    return (nums[upper_median_index] + nums[upper_median_index - 1]) / 2


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
            #  ||   |=====|
            # nums1  nums2
            if DEBUGGING:
                print("nums1 = [] shortcut")
            return median_of_sorted(nums2, nums2_length)
        if nums2_length == 0:
            # |---------|  ||
            #    nums1    nums2
            if DEBUGGING:
                print("nums2 = [] shortcut")
            return median_of_sorted(nums1, nums1_length)

        combined_length = nums1_length + nums2_length
        nums1_lower = nums1[0]
        nums1_upper = nums1[-1]
        nums2_lower = nums2[0]
        nums2_upper = nums2[-1]

        # if the two overlap at at most 1 value, then the median is the median
        # of the concatenation two lists
        if nums1_upper <= nums2_lower:
            # |---------||=====|
            #    nums1    nums2
            if DEBUGGING:
                print("|-nums1-||=nums2=|")
            return median_of_sorted([*nums1, *nums2], combined_length)

        if nums2_upper <= nums1_lower:
            # |=====||---------|
            #  nums2    nums1
            if DEBUGGING:
                print("|=nums2=||-nums1-|")
            return median_of_sorted([*nums2, *nums1], combined_length)

        # one of
        # |---------|+++|=====|
        #    nums1       nums2
        # |=====|+++|---------|
        #  nums2       nums1
        # |------|+++++|------|
        #  nums1  nums2
        # |======|+++++|======|
        #  nums2  nums1
        # ||+++++++++++++||
        #   nums1 nums2

        nums1_index = 0
        nums2_index = 0
        nums1_value = nums1_lower
        nums2_value = nums2_lower
        combined: List[int] = []

        while nums1_index + nums2_index <= combined_length - 2:
            if nums1[nums1_index] <= nums2[nums2_index]:
                combined.append(nums1[nums1_index])
                nums1_index += 1
            else:
                combined.append(nums2[nums2_index])
                nums2_index += 1

            if nums1_index + 1 == nums1_length:
                if DEBUGGING:
                    print("exhausted nums1")
                combined.extend(nums2[nums2_index:])
                break
            if nums2_index + 1 == nums2_length:
                if DEBUGGING:
                    print("exhausted nums2")
                combined.extend(nums1[nums1_index:])
                break


        if DEBUGGING:
            print(f"combined -> {combined}")
        return median_of_sorted(combined, combined_length)
