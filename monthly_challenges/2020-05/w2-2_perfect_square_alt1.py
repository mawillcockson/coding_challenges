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

# Submitted:
# https://leetcode.com/submissions/detail/341903032/?from=/explore/challenge/card/may-leetcoding-challenge/535/week-2-may-8th-may-14th/3324/
# https://leetcode.com/submissions/detail/341905412/?from=/explore/challenge/card/may-leetcoding-challenge/535/week-2-may-8th-may-14th/3324/

from collections import defaultdict
from typing import Iterator, DefaultDict, Callable

# ceil(sqrt(2**31 - 1))
max_sqrt = 46341


def squares_gen() -> DefaultDict[int, bool]:
    squares: DefaultDict[int, bool] = defaultdict(bool)
    for num in range(max_sqrt):
        squares[num ** 2] = True

    return squares


squares = squares_gen()


class Solution:
    def isPerfectSquare(self, num: int) -> bool:
        return squares[num]
