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

import random
import sys
from copy import copy
from itertools import chain
from typing import (
    TYPE_CHECKING,
    Callable,
    Dict,
    List,
    NamedTuple,
    NewType,
    Optional,
    Sequence,
    Set,
    TypeVar,
    Union,
)

T = TypeVar("T")
Sea = List[List[str]]


class Coordinate(NamedTuple):
    row: int
    column: int


Island = NewType("Island", Set[Coordinate])
Color = NewType("Color", str)

DEBUGGING = True

if DEBUGGING or TYPE_CHECKING:
    import rich
    from rich.color import ANSI_COLOR_NAMES

    distinct_color_names = [
        "dodger_blue2",
        "green3",
        "deep_sky_blue1",
        "dark_red",
        "deep_pink4",
        "purple4",
        "red3",
        "dark_orange3",
        "magenta2",
        "light_cyan1",
        "gold1",
    ]
    random_colors: List[Color] = []
    for color_name in distinct_color_names:
        assert color_name in ANSI_COLOR_NAMES, f"{color_name} not an ANSI color name"
        random_colors.append(Color(color_name))
    random.shuffle(random_colors)

    def new_random_color_gen() -> Callable[[], Color]:
        random_color_iter = iter(random_colors)

        def random_color() -> Color:
            return next(random_color_iter)

        return random_color

    def show_sea_progress(
        sea: Sea,
        islands: List[Island],
        island_colors: Dict[int, Color],
        width: int,
        current_row_index: int,
    ) -> None:
        point_colors: Dict[Coordinate, Color] = {}
        for island_index, island in enumerate(islands):
            color = island_colors[island_index]
            point_colors.update((coordinate, color) for coordinate in island)
        for row_index, row in enumerate(sea):
            for column_index, value in enumerate(row):
                coordinate = Coordinate(row_index, column_index)
                color = point_colors.get(coordinate, Color("black"))
                rich.print(f"[white on {color}] [/white on {color}]", end="")

            if current_row_index == row_index:
                print("<")
            else:
                print("")

        print("-" * width)


def number_of_islands(sea: Sea) -> int:
    number_of_rows = len(sea)
    assert number_of_rows >= 1, "empty sea"
    width = len(sea[0])

    for row in sea:
        assert len(row) == width, "non-rectangular sea"

    islands: List[Island] = []
    if DEBUGGING:
        random_color = new_random_color_gen()
        island_colors: Dict[int, Color] = {0: random_color()}
    for row_index, row in enumerate(sea):
        # is any new land adjacent to current islands?
        for column_index, value in enumerate(row):
            if not int(value):
                continue

            land = Coordinate(row_index, column_index)
            left_coordinate = Coordinate(land.row, land.column + 1)
            right_coordinate = Coordinate(land.row, land.column - 1)
            above_coordinate = Coordinate(land.row - 1, land.column)

            adjacent_island_indeces: List[int] = []
            for island_index, island in enumerate(islands):
                if (
                    left_coordinate in island
                    or right_coordinate in island
                    or above_coordinate in island
                ):
                    adjacent_island_indeces.append(island_index)

            if adjacent_island_indeces:

                if DEBUGGING:
                    new_island_index = len(islands) - len(adjacent_island_indeces) + 1
                    first_color = island_colors[adjacent_island_indeces[0]]
                    island_colors[new_island_index] = first_color
                    for island_index in adjacent_island_indeces:
                        del island_colors[island_index]
                    # rich.print(island_colors)
                    island_colors = dict(
                        zip(
                            range(len(island_colors)),
                            (
                                color
                                for island_index, color in sorted(island_colors.items())
                            ),
                        )
                    )
                    # rich.print(island_colors)

                adjacent_island_indeces.append(len(islands))
                islands.append(Island({land}))
                new_island = Island(
                    set(
                        chain.from_iterable(
                            islands[island_index]
                            for island_index in adjacent_island_indeces
                        )
                    )
                )
                for island_index in reversed(adjacent_island_indeces):
                    islands.pop(island_index)
                islands.append(new_island)

            else:

                if DEBUGGING:
                    new_island_index = len(islands)
                    color = random_color()
                    island_colors[new_island_index] = color
                    # print(island_colors)

                islands.append(Island({land}))

        if DEBUGGING:
            show_sea_progress(
                sea=sea,
                islands=islands,
                island_colors=island_colors,
                width=width,
                current_row_index=row_index,
            )
            # print(islands)

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
        TestCase(
            [
                [1, 1, 0],
                [0, 0, 1],
            ],
            2,
        ),
    ]

    for test_number, test in enumerate(test_cases):
        print(f"test #{test_number + 1}")
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
