"""
There are 2N people a company is planning to interview. The cost of flying the i-th person to city A is costs[i][0], and the cost of flying the i-th person to city B is costs[i][1].

Return the minimum cost to fly every person to a city such that exactly N people arrive in each city.

 

Example 1:

Input: [[10,20],[30,200],[400,50],[30,20]]
Output: 110
Explanation: 
The first person goes to city A for a cost of 10.
The second person goes to city A for a cost of 30.
The third person goes to city B for a cost of 50.
The fourth person goes to city B for a cost of 20.

The total minimum cost is 10 + 30 + 50 + 20 = 110 to have half the people interviewing in each city.

 

Note:

    1 <= costs.length <= 100
    It is guaranteed that costs.length is even.
    1 <= costs[i][0], costs[i][1] <= 1000
"""
# The solution can't be as simple as:
#
# >>> from itertools import starmap
# >>> from random import randint
# >>> rcost=lambda:randint(1,1000)
# >>> rcosts=lambda x:[[rcost(), rcost()] for i in range(x)]
# >>> costs=rcosts(5)
# >>> costs
# [[253, 961], [41, 626], [249, 118], [197, 610], [32, 198]]
# >>> sum(starmap(min, costs))
# 641
#
# because there needs to be an even number of people in each city.
# Breaking example:
#
# costs = [[1, 10], [1, 10]]
# Correct answer is 11

from collections import Counter
from typing import Dict, List, TypedDict


class Candidate(TypedDict):
    prices: List[int]
    city: int


Candidates = Dict[int, Candidate]

def balance(candidates: Candidates) -> None:
    city_counts = Counter(candidates[candidate]["city"] for candidate in candidates)
    ordered_cities = city_count.most_common()
    if not ordered_cities:
        return

    counts = [count for (city, count) in ordered_cities]
    if len(counts) == len(set(counts))
        return

    

def candidate_cost(candidate: Candidate) -> int:
    return candidate["prices"][candidate["city"]]


class Solution:
    def twoCitySchedCost(self, costs: List[List[int]]) -> int:
        if not costs:
            raise ValueError("Cannot use empty list")

        candidates: Candidates = dict()
        for candidate, prices in enumerate(costs):
            min_cost = min(prices)
            city = prices.index(min_cost)
            candidates[candidate] = Candidate(
                {"prices": prices, "city": city,}
            )

        balance(candidates)

        return sum(map(candidate_cost, candidates.values()))
