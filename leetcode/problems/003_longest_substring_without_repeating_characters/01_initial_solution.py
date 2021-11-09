"""
https://leetcode.com/problems/longest-substring-without-repeating-characters/

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

- 0 <= s.length <= 5 * 10^4 (50_000)
- s consists of English letters, digits, symbols and spaces


Note:

This is similar to problem solved by KMP:
https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm

Submitted: https://leetcode.com/submissions/detail/578954540/
"""
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import Dict, List

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
        current_substring_characters: "Dict[str, int]" = {}
        max_unique_substring_length = 0
        current_substring: "List[str]" = []
        longest_unique_substring = current_substring
        word_index = -1

        for character in s:
            word_index += 1
            # print(f"...{character}", end="")  # debug
            if character in current_substring_characters:
                # print("!")  # debug
                # current substring is no longer unique if this character is added
                max_unique_substring_length = max(
                    max_unique_substring_length, len(current_substring_characters)
                )
                if len(current_substring) > len(longest_unique_substring):
                    longest_unique_substring = current_substring

                # where does this character occur in the current_substring?
                character_index = current_substring_characters[character]

                # drop all characters with indeces up to an including that position
                # print(f"{current_substring} -> ", end="")  # debug
                current_substring = current_substring[character_index + 1 :]
                current_substring.append(character)
                # print(repr(current_substring))  # debug

                # print(f"{current_substring_characters} -> ", end="")  # debug
                current_substring_characters = dict(
                    zip(current_substring, range(len(current_substring)))
                )
                # print(current_substring_characters)  # debug

                # print(f"word_index => {word_index} -> ", end="")  # debug
                word_index = word_index - character_index - 1
                # print(word_index)  # debug
                if word_index < 0:
                    print("\n")
                    import pprint

                    pprint.pprint(locals())
                    raise Exception
                continue

            current_substring_characters[character] = word_index
            current_substring.append(character)
            # print("")  # debug

        if len(current_substring_characters) > max_unique_substring_length:
            max_unique_substring_length = len(current_substring)
            longest_unique_substring = current_substring

        if not TYPE_CHECKING and Answer is int:
            return max_unique_substring_length

        return Answer(
            substring="".join(longest_unique_substring),
            length=max_unique_substring_length,
        )
