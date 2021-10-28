"""
Given a string s, find the length of the longest substring without repeating characters.

Example 1:

Input: s = "abcabcbb"
Output: 3
Explanation: The answer is "abc", with the length of 3.

Example 2:

Input: s = "bbbbb"
Output: 1
Explanation: The answer is "b", with the length of 1.

Example 3:

Input: s = "pwwkew"
Output: 3
Explanation: The answer is "wke", with the length of 3.
Notice that the answer must be a substring, "pwke" is a subsequence and not a substring.

Example 4:

Input: s = ""
Output: 0

 

Constraints:

    0 <= s.length <= 5 * 10^4 (50_000)
    s consists of English letters, digits, symbols and spaces.
"""
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import List, Set

    from test_solution import Answer


if not TYPE_CHECKING and "Answer" not in globals():
    try:
        from test_solution import Answer
    except ImportError:

        Answer = int


class Solution:
    "required by leetcode"

    def lengthOfLongestSubstring(self, s: str) -> "Answer":
        "longest substring without repeating characters"
        current_substring_characters: "Set[str]" = set()
        max_unique_substring_length = 0
        current_substring: "List[str]" = []
        longest_unique_substring = current_substring
        for character in s:
            if character in current_substring_characters:
                # current substring is no longer unique if this character is added
                max_unique_substring_length = max(
                    max_unique_substring_length, len(current_substring_characters)
                )
                if len(current_substring) > len(longest_unique_substring):
                    longest_unique_substring = current_substring

                # reset everything
                current_substring_characters = {character}
                current_substring = [character]
                continue

            current_substring_characters.add(character)
            current_substring.append(character)

        if not TYPE_CHECKING and Answer is int:
            return max_unique_substring_length

        return Answer(
            substring="".join(longest_unique_substring),
            length=max_unique_substring_length,
        )
