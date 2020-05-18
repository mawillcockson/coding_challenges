"""
Given an array of size n, find the majority element. The majority element is
the element that appears more than ⌊ n/2 ⌋ times.

You may assume that the array is non-empty and the majority element always
exist in the array.

Example 1:

    Input: [3,2,3] Output: 3

    Example 2:

    Input: [2,2,1,1,1,2,2] Output: 2

"""
from math import floor
from collections import Counter

# NOTE: it feels like cheating to not do really any algorithmic thinking,
# and just take advantage of the Python standard library
class Solution:
    def majorityElement(self, nums: List[int]) -> int:
        return Counter(nums).most_common(1)[0][0]


class Solution:
    def majorityElement(self, nums: List[int]) -> int:
        
# I saw this in the solutions, listed as having run very quickly:
class Solution:
    def majorityElement(self, nums: List[int]) -> int:
        nums.sort()
        return nums[len(nums)//2]

# It's very clever. It relies on the constraint that there will be only a
# single, strong majority element, in which case a sorted version of the array
# would have that element in the middle: If more than half the array is the
# same element, it will necessarily show up in the middle of a sorted array:
# [1, 1, 1, 1, 1, 8, 8, 8, 8]
#              ^
