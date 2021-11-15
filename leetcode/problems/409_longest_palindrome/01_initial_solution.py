"""
https://leetcode.com/problems/longest-palindrome/

Given a string s which consists of lowercase or uppercase letters, return the
length of the longest palindrome that can be built with those letters.

Letters are case sensitive, for example, "Aa" is not considered a palindrome
here.

Example 1:
Input: s = "abccccdd"
Output: 7
Explanation:
One longest palindrome that can be built is "dccaccd", whose length is 7.

Example 2:
Input: s = "a"
Output: 1

Example 3:
Input: s = "bb"
Output: 2

Constraints:

- 1 <= s.length <= 2000
- s consists of lowercase and/or uppercase English letters only.
"""
from typing import TYPE_CHECKING, Counter
from collections import Counter


DEBUGGING = True

try:
    # pylint: disable=unused-import
    import test_solution
except ImportError:
    DEBUGGING = False

class Solution:
    "required by leetcode"
    def longestPalindrome(self, s: str) -> int:
        """
        return the length of the longest palindrome constructible using only
        the characters in s
        """
        pass
