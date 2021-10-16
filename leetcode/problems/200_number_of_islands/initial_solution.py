"""
https://leetcode.com/problems/number-of-islands/

Given an m x n 2D binary grid grid which represents a map of '1's (land) and
'0's (water), return the number of islands.

An island is surrounded by water and is formed by connecting adjacent lands
horizontally or vertically. You may assume all four edges of the grid are all
surrounded by water.

Example 1:

Input: grid = [
  ["1","1","1","1","0"],
  ["1","1","0","1","0"],
  ["1","1","0","0","0"],
  ["0","0","0","0","0"]
]

Output: 1

Example 2:

Input: grid = [
  ["1","1","0","0","0"],
  ["1","1","0","0","0"],
  ["0","0","1","0","0"],
  ["0","0","0","1","1"]
]

Output: 3

Constraints:

- m == grid.length
- n == grid[i].length
- 1 <= m, n <= 300
- grid[i][j] is '0' or '1'.
"""

import sys
from copy import copy
from itertools import chain
from typing import List, NamedTuple, NewType, Sequence, Set, TypeVar, Union

T = TypeVar("T")
Sea = List[List[str]]


class Coordinate(NamedTuple):
    row: int
    column: int


Island = NewType("Island", Set[Coordinate])


def number_of_islands(sea: Sea) -> int:
    number_of_rows = len(sea)
    assert number_of_rows >= 1, "empty sea"
    width = len(sea[0])

    for row in sea:
        assert len(row) == width, "non-rectangular sea"

    islands: List[Island]

    for row_index, row in enumerate(sea):
        new_land: Set[Coordinate] = set()
        for column_index, value in enumerate(row):
            if int(value):
                new_land.add(Coordinate(row_index, column_index))

        if row_index == 0:
            islands = [Island({land}) for land in new_land]
            continue

        # is any new land adjacent to current islands?
        for land in new_land:
            left_coordinate = Coordinate(land.row, land.column + 1)
            right_coordinate = Coordinate(land.row, land.column - 1)
            above_coordinate = Coordinate(land.row - 1, land.column)

            new_islands = copy(islands)
            adjacent_islands: List[Island] = []
            for island_index, island in enumerate(islands):
                if (
                    left_coordinate in island
                    or right_coordinate in island
                    or above_coordinate in island
                ):
                    adjacent_islands.append(island)
                    new_islands.pop(island_index)

            new_island = Island(set(chain.from_iterable(adjacent_islands)))
            new_islands.append(new_island)
            islands = new_islands

    return len(islands)


if __name__ == "__main__":

    def stringify(ints: Union[List[List[int]], Sea]) -> Sea:
        return [[str(value) for value in row] for row in ints]

    class TestCase(NamedTuple):
        case: Union[List[List[int]], Sea]
        correct_answer: int

    test_cases = [
        TestCase(
            [
                [0, 0, 0],
                [0, 1, 0],
                [0, 0, 0],
            ],
            1,
        ),
        TestCase(
            [
                [1, 0, 1],
                [0, 1, 0],
                [1, 0, 1],
            ],
            5,
        ),
        TestCase(
            [
                [1, 1, 1],
                [1, 1, 1],
                [1, 1, 1],
            ],
            1,
        ),
        TestCase(
            [
                [1],
            ],
            1,
        ),
    ]

    for test in test_cases:
        answer = number_of_islands(stringify(test.case))
        if answer != test.correct_answer:
            print("[")
            for row in test.case:
                print(f" {row}")
            print("]")
            print(f"incorrect: {answer}")
            sys.exit(1)

    print("tests passed")


class Solution:
    def numIslands(self, grid: List[List[str]]) -> int:
        return number_of_islands(grid)
