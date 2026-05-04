/* 796. Rotate Strings
Given two strings s and goal, return true if and only if s can become goal after some number of shifts on s.

A shift on s consists of moving the leftmost character of s to the rightmost position.

For example, if s = "abcde", then it will be "bcdea" after one shift.

Example 1:
Input: s = "abcde", goal = "cdeab"
Output: true

Example 2:
Input: s = "abcde", goal = "abced"
Output: false

Constraints:
- 1 <= s.length, goal.length <= 100
- s and goal consist of lowercase English letters.
*/
/**
 * @param {string} s
 * @param {string} goal
 * @return {boolean}
 */
const rotateString = (s, goal) => {
  const s_length = s.length;
  const goal_length = goal.length;

  if (s_length === 0) return true;
  if (goal_length === 0) return false;
  if (s_length !== goal_length) return false;

  const ss = Array.from(s);
  for (let i = 0; i <= s_length; ++i) {
    ss.unshift(ss.pop());
    if (ss.join("") === goal) return true;
  }
  return false;
};

const tests = () => {
  for (const {
    input: { s, goal },
    output,
  } of [
    { input: { s: "", goal: "" }, output: true },
    { input: { s: "a", goal: "" }, output: false },
    { input: { s: "", goal: "a" }, output: true },
    { input: { s: "a", goal: "ab" }, output: false },
    { input: { s: "ab", goal: "dbac" }, output: false },
    { input: { s: "aaab", goal: "baaa" }, output: true },
    { input: { s: "aaba", goal: "baaa" }, output: true },
    { input: { s: "abc", goal: "bca" }, output: true },
    { input: { s: "abc", goal: "cba" }, output: false },
    { input: { s: "abcde", goal: "cdeab" }, output: true },
    { input: { s: "abcde", goal: "abced" }, output: false },
    { input: { s: "abcde", goal: "abcde" }, output: true },
  ]) {
    const result = rotateString(s, goal);
    if (result === output) {
      console.log("expected === result: %o", result);
    } else {
      console.error("problem with case: %o -> %o", { s, goal }, result);
    }
  }
};

if (import.meta.url.endsWith("rotate_strings.js")) {
  tests();
}
