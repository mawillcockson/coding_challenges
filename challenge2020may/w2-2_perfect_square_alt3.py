r"""
Given a positive integer num, write a function which returns True if num is a perfect square else False.

Follow up: Do not use any built-in library function such as sqrt.


Example 1:

Input: num = 16
Output: true

Example 2:

Input: num = 14
Output: false


Constraints:

    1 <= num <= 2^31 - 1
"""

# Following suggestions from:
# https://en.wikipedia.org/wiki/Integer_square_root#Using_only_integer_division

# Submitted:
# https://leetcode.com/submissions/detail/341917485/?from=/explore/challenge/card/may-leetcoding-challenge/535/week-2-may-8th-may-14th/3324/

from math import floor
from time import sleep


class Solution:
    def isPerfectSquare(self, num: int) -> bool:
        x = num
        step = lambda x: floor((x + floor(num / x)) / 2)
        while abs(x - (x := step(x))) >= 1:
            print(f"{x}")
            sleep(1)
            continue

        return x ** 2 == num

