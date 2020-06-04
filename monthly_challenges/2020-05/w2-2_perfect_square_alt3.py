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
from typing import Dict, List
from time import sleep


class Solution:
    cache: Dict[int, int] = dict()

    def isPerfectSquare(self, num: int) -> bool:
        if num <= 1:
            return True

        step = lambda x: floor((x + floor(num / x)) / 2)

        chain: List[int] = list()

        def sqrt(x: int) -> int:

            print(f"{x}")
            sleep(1)

            if x in self.cache:
                return self.cache[x]
            elif abs(x - (x := step(x))) <= 1:
                for n in chain:
                    self.cache[n] = x
                chain.clear()
                self.cache[num] = x
                return x
            else:
                chain.append(x)
                return sqrt(step(x))

        return sqrt(num) ** 2 == num


# Testing
from collections import defaultdict
from typing import DefaultDict
from math import ceil, sqrt

is_square = Solution().isPerfectSquare
max_sqrt = ceil(sqrt(2 ** 31 - 1))
squares: DefaultDict[int, bool] = defaultdict(bool)
for num in range(max_sqrt):
    squares[num ** 2] = True

for num in range(2 ** 31 - 1):
    if num % 1000 == 0:
        print(f"Checking {num}")
    if is_square(num) != squares[num]:
        print(f"Should return {squares[num]}: {num}")
