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
# https://leetcode.com/submissions/detail/341896487/?from=/explore/challenge/card/may-leetcoding-challenge/535/week-2-may-8th-may-14th/3324/

from collections import deque
from typing import Iterator, Deque, Callable

# ceil(sqrt(2**31 - 1))
max_sqrt = 46341


def squares_gen() -> Callable[[], Iterator[int]]:
    cache: Deque[int] = deque()
    squares: Iterator[int] = (x ** 2 for x in range(max_sqrt))

    def gen() -> Iterator[int]:
        for num in cache:
            # print(f"Pulling {num} from cache")
            yield num
        for num in squares:
            cache.append(num)
            yield num

    return gen


squares = squares_gen()


class Solution:
    def isPerfectSquare(self, num: int) -> bool:
        for square in squares():
            if square == num:
                return True
            elif square > num:
                return False
            else:
                continue

        return False
