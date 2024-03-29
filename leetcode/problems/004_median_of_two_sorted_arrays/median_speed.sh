#!/bin/sh
set -eu
SETUP='
from typing import List
def median_of_sorted(nums: List[int]) -> float:
    length = len(nums)
    if length == 1:
        return float(nums[0])

    if length % 2 == 1:  # is odd
        return float(nums[length // 2])
    
    upper_median_index = length // 2
    return (nums[upper_median_index] + nums[upper_median_index - 1]) / 2

from statistics import median
l1 = [1, 2, 3]
l2 = [1, 2, 3, 4]
l3 = [1] * 2_000
l4 = [1]

for l in [l1, l2, l3, l4]:
    guess = median_of_sorted(l)
    correct = float(median(l))
    assert guess == correct, f"{l} -> {guess} != {correct}"
'
python -m timeit -s "${SETUP}" 'median(l1);median(l2);median(l3);median(l4)'
python -m timeit -s "${SETUP}" 'median_of_sorted(l1);median_of_sorted(l2);median_of_sorted(l3);median_of_sorted(l4)'
