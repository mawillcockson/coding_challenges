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
        right_median_index = (combined_length - 2) // 2

        # <= is necessary, and < won't work because they could be equal, in
        # which case it doesn't matter which one is chosen as left_median, and
        # which one is chosen as right_median
        if nums1_lower <= nums2_lower:
            left_median = nums1_lower
            right_median = nums2_lower
        else:
            left_median = nums2_lower
            right_median = nums1_lower

        if DEBUGGING:
            print(f"nums1 -> {nums1}")
            print(f"nums2 -> {nums2}")
            print(f"left_median  -> {left_median}")
            print(f"right_median -> {right_median}")

        while True:
            if nums1_index + nums2_index == right_median_index:
                if DEBUGGING:
                    print("reached median index")
                if combined_length % 2 == 0:  # is even
                    return (left_median + right_median) / 2
                return float(right_median)

            if nums1_index + 1 == nums1_length:
                if DEBUGGING:
                    print("exhausted nums1")
                if combined_length % 2 == 0:
                    return (
                        nums2[right_median_index - nums1_index]
                        + nums2[right_median_index - nums1_index - 1]
                    ) / 2
                return float(nums2[right_median_index - nums1_index])

            if nums2_index + 1 == nums2_length:
                if DEBUGGING:
                    print("exhausted nums2")
                if combined_length % 2 == 0:
                    return (
                        nums1[right_median_index - nums2_index]
                        + nums1[right_median_index - nums2_index - 1]
                    ) / 2
                return float(nums1[right_median_index - nums2_index])

            if nums1_value >= nums2_value:
                nums2_index += 1
                nums2_value = nums2[nums2_index]
            else:
                nums1_index += 1
                nums1_value = nums1[nums1_index]

            if nums1_value >= nums2_value:
                left_median = nums2_value
                right_median = nums1_value
            else:
                left_median = nums1_value
                right_median = nums2_value

            if DEBUGGING:
                print(f"nums1_value  -> {nums1_value}")
                print(f"nums2_value  -> {nums2_value}")
                print(f"left_median  -> {left_median}")
                print(f"right_median -> {right_median}")
