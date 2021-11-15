# pylint: disable=invalid-name
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
from collections import Counter
from typing import TYPE_CHECKING

DEBUGGING = True

try:
    # pylint: disable=unused-import
    import test_solution
except ImportError:
    DEBUGGING = False


class Solution:
    "required by leetcode"
    # pylint: disable=too-few-public-methods

    def longestPalindrome(self, s: str) -> int:
        """
        return the length of the longest palindrome constructible using only
        the characters in s
        """
        # I am pretty sure collections.Counter will be indespensible here.
        # My observation is that a Counter for a palindrome would probably
        # contain at most one odd-count element:
        # "abcba" ->
        # {
        #   "a": 2,
        #   "b": 2,
        #   "c": 1,
        # }
        # Another observation is that a palindrome of even length implies that
        # the string from which is was constructed contained at modt one
        # odd-count letter.
        #
        # In this vein, a palindrome can be constructed by separating all one 1-count letters.
        # Then, the odd-count letters are reduced by one, with the letter taken
        # off added to the 1-count letters (i.e.:
        # {
        #   "a": 3,
        #   "b": 5,
        #   "c": 4,
        #   "d": 2,
        # }
        # {
        #   "e": 1,
        #   "f": 1,
        # }
        # ->
        # {
        #   "a": 2,
        #   "b": 4,
        #   "c": 4,
        #   "d": 2,
        # }
        # {
        #   "e": 1,
        #   "f": 1,
        #   "a": 1,
        #   "b": 1,
        # }
        # Then, the counts of the even ones can be halved and summed, and if
        # there are any 1-count letters, the sum can be incremented.