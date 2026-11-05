/*
3871. Count Commas in Range II

You are given an integer n.

Return the total number of commas used when writing all integers from [1, n]
(inclusive) in standard number formatting.

In standard formatting:
- A comma is inserted after every three digits from the right.
- Numbers with fewer than 4 digits contain no commas.

Examples:

Input: n = 1002
Output: 3
Explanation:
The numbers "1,000", "1,001", and "1,002" each contain one comma, giving a
total of 3.

Input: n = 998
Output: 0
Explanation:
All numbers from 1 to 998 have fewer than four digits. Therefore, no commas are
used.

Constraints:
- 1 <= n <= 10^15
 */

/* Thoughts:
 * The number of commas in a number don't change until it crosses a boundary:
 * every 3 orders of magnitude, starting at 4. The order of magnitude is
 * determined by floor(log10(x)).
 *
 * This means that for all numbers between 1,000 and 999,999 each number only
 * has 1 comma, and the number of commas is equal to how many numbers there
 * are.
 *
 * I think the number of commas in a single number can be calculated by:
 * floor((log10(n) - 1) / 3)
 *
 * This seems like it holds for any contiguous range of numbers that don't
 * cross this boundary.
 *
 * Actually, it's just floor(log10(n) / 3) as 10^0 is 1.
 *
 * The total commas up to a number n is the sum of the number of commas in each
 * number, from 1 to n.
 *
 * But the total commas from 10^(floor(log10(n) / 3) * 3) to n is just the
 * number of numbers, multiplied by the number of commas in each number, since
 * that's the same.
 *
 * For instance, if n = 100,000, then the total commas in the range from 1,000
 * to 100,000 is: (100,000 - 1,000) * 1 = 99,000
 *
 * I'll test if this holds.
 *
 * In the example I gave, it's actually 99,001 commas, as the number n should
 * also be counted.
 */

const log10 = Math.log10;
const floor = Math.floor;
const pow = Math.pow;
const numberFormat = new Intl.NumberFormat("en-US", {
  numberingSystem: "latn",
});
const formatNumber = numberFormat.format;

export const commasAtNumbers = new Map<number, number>([
  [999, 0],
  [999_999, 999_000],
  [999_999_999, 1_998_999_000],
  [999_999_999_999, 2_998_998_999_000],
]);

export function countCommas(n: number): number {
  const commas_in_n = commaCount(n);
  if (commas_in_n < 1) return 0;
  const smallest_number_with_same_comma_count = pow(10, commas_in_n * 3);
  const commas_at_this_number_of_commas =
    (n - smallest_number_with_same_comma_count + 1) * commas_in_n;
  let total_commas_from_lower_comma_ranges = commasAtNumbers.get(
    smallest_number_with_same_comma_count - 1,
  );
  if (total_commas_from_lower_comma_ranges === undefined) {
    total_commas_from_lower_comma_ranges = countCommas(
      smallest_number_with_same_comma_count - 1,
    );
    commasAtNumbers.set(
      smallest_number_with_same_comma_count - 1,
      total_commas_from_lower_comma_ranges,
    );
  }
  return commas_at_this_number_of_commas +
    total_commas_from_lower_comma_ranges;
}

/** slow but correct way to count the commas in a number
 */
function referenceCommaCount(n: number): number {
  return occurences(",", formatNumber(n));
}

/** count substrings in a string
 */
function occurences(substring: string, str: string): number {
  let count = 0;
  let position = 0;
  const substringLength = substring.length;
  if (substringLength <= 0) throw new Error(`substring is empty: ${substring}`);
  while (true) {
    position = str.indexOf(substring, position);
    if (position < 0) break;
    ++count;
    position += substringLength;
  }
  return count;
}

/** count total commas in all numbers from 1 to n
 */
export function commaCount(n: number): number {
  const magnitude = log10(n);
  //if (magnitude <= 2) return 0;
  //if (3 <= magnitude && magnitude <= n) return n;
  if (isNaN(magnitude)) throw new Error("nope");
  if (magnitude === 0) return 0;
  return floor(magnitude / 3);
}

/** generate a sequence of numbers from 0 to n inclusive without allocating an
 * array
 */
export function* countTo(n: number, from?: number): Iterable<number> {
  let i = 0;
  if (typeof from !== "undefined") i = from;
  while (i <= n) {
    yield i;
    ++i;
  }
}

/** slow but correct way of counting the total commas in all numbers from 0 to n
 */
function referenceCountCommas(n: number): number {
  let commas = 0;
  for (const curr of countTo(n)) {
    commas += referenceCommaCount(curr);
  }
  return commas;
}

function commasAtThisMagnitude(n: number): number {
  const commas = commaCount(n);
  const smallestNumberWithSameCommaCount = pow(10, commas * 3);
  return (n - smallestNumberWithSameCommaCount + 1) * commas;
}

export function randBetween(max: number): number;
export function randBetween(min: number, max: number): number;
export function randBetween(minOrMax: number, max?: number): number {
  const maxNum = max === undefined ? minOrMax : max;
  const minNum = max === undefined ? 0 : minOrMax;
  if (maxNum < minNum) {
    throw new Error(`max (${maxNum}) is less than min (${minNum})`);
  }
  if (maxNum === minNum) return minNum;
  const maxInt = floor(maxNum);
  const minInt = Math.ceil(minNum); // we don't want a number outside the desired range
  return floor(Math.random() * (maxInt - minInt + 1) + minInt);
}

if ("Deno" in globalThis && Deno.env.get("NU_VERSION")) {
  const expect = (await import("jsr:@std/expect")).expect;

  Deno.test.each([
    ["ab", 0],
    ["a,b", 1],
    ["a,b,c", 2],
    ["a,,b", 2],
    [",,ab", 2],
    ["ab,,", 2],
    [",a,b,c,,d,,", 7],
  ])("count commas in: %i == %i", (str: string, count: number) => {
    expect(occurences(",", str)).toEqual(count);
  });

  Deno.test("Example 1 (reference)", () => {
    expect(referenceCountCommas(1_002)).toEqual(3);
  });

  Deno.test("Exmple 2 (reference)", () => {
    expect(referenceCountCommas(998)).toEqual(0);
  });

  Deno.test.each([
    999,
    1_000,
    1_001,
    pow(10, 14) - 1,
    pow(10, 14),
    pow(10, 14) + 1,
    pow(10, 15) - 1,
    ...Array.from(Array(15).keys()).map((power) => pow(10, power)),
  ])("check commaCount(%i)", (n: number) => {
    expect(commaCount(n)).toEqual(referenceCommaCount(n));
  });

  Deno.test.each([1_000, 10_000, 100_000])(
    "how many commas at a magnitude: %i",
    (n: number) => {
      expect(commasAtThisMagnitude(n)).toEqual(referenceCountCommas(n));
    },
  );

  Deno.test("Example 1", () => {
    expect(countCommas(1_002)).toEqual(3);
  });

  Deno.test("Exmple 2", () => {
    expect(countCommas(998)).toEqual(0);
  });

  Deno.test.each([12_345, 1_234_567 //randBetween(pow(10, 7), pow(10, 8))
  ])(
    "big checks (%i)",
    (n: number) => {
      expect(countCommas(n)).toEqual(referenceCountCommas(n));
    },
  );

  Array.from(Array(16).keys()).forEach((p) => countCommas(pow(10, p)));
  console.dir(commasAtNumbers);
}
