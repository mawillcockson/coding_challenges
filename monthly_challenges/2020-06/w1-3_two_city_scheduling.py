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

from collections import Counter, deque
from math import floor
from random import randint
from typing import Callable, Deque, Dict, List, Tuple, TypedDict
from warnings import warn


class Candidate(TypedDict):
    prices: List[int]
    city: int
    index: int


Candidates = Dict[int, Candidate]


def candidate_cost(candidate: Candidate) -> int:
    return candidate["prices"][candidate["city"]]


def next_best_choice(candidate: Candidate) -> int:
    ordered_prices = sorted(candidate["prices"])
    if len(ordered_prices) <= candidate["city"]:
        return candidate["prices"][-1]
    else:
        return candidate["prices"].index(ordered_prices[candidate["city"] + 1])


def sort_choices(candidate: Candidate, cities: List[int]) -> List[Tuple[int, int]]:
    num_cities = len(candidate["prices"])
    if any(map(lambda x: x > num_cities, cities)):
        raise ValueError(f"Cities out of range: {cities}")

    return sorted((candidate["prices"][city], city) for city in cities)

def best_candidate(candidates: List[Candidate], cities: List[Tuple[int, int]]) -> Tuple[int, int]:
    pass


def balance(candidates: Candidates) -> None:
    city_counts = Counter(candidates[candidate]["city"] for candidate in candidates)
    ordered_cities = city_counts.most_common()
    if not ordered_cities:
        return

    # If all are the same
    counts = [count for (city, count) in ordered_cities]
    if len(counts) == len(set(counts)):
        return

    students_per_city = len(candidates) / len(ordered_cities)
    s_p_c = floor(students_per_city)
    if s_p_c != students_per_city:
        warn(f"The number of students doesn't divide cleanly into the number of cities")

    excess = [
        (city, count - s_p_c) for (city, count) in ordered_cities if count - s_p_c > 1
    ]
    excess_cities = [city for (city, count) in excess]
    dearth = [
        (city, s_p_c - count) for (city, count) in ordered_cities if s_p_c - count > 1
    ]
    excess_candidates: Deque[Candidate] = deque(
        [
            candidates[candidate]
            for candidate in candidates
            if candidates[candidate]["city"] in excess_cities
        ]
    )
    for 


def candidate_choices(costs: List[List[int]]) -> Candidates:
    if not costs:
        raise ValueError("Cannot use empty list")

    candidates: Candidates = dict()
    for candidate, prices in enumerate(costs):
        min_cost = min(prices)
        city = prices.index(min_cost)
        candidates[candidate] = Candidate(
            {"prices": prices, "city": city, "index": candidate}
        )

    balance(candidates)

    return candidates


class Solution:
    def twoCitySchedCost(self, costs: List[List[int]]) -> int:
        candidates = candidate_choices(costs)

        return sum(map(candidate_cost, candidates.values()))


# Testing
rcost: Callable[[], int] = lambda: randint(1, 1000)
rcosts: Callable[[int, int], List[List[int]]] = lambda x, y: [
    [rcost() for i in range(y)] for j in range(x)
]
c = Solution().twoCitySchedCost
print(
    f"""
{c([[1,10], [1,10]])}
{c([1,10])}
"""
)
for i in range(10):
    print(
        f"""
costs: {(costs := rcosts(randint(2,4), randint(1,4)))}
candidates: {(candidates := candidate_choices(costs))}
total cost: {c(costs)}
"""
    )
