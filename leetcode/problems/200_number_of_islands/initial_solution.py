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
    Iterable,
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
    from collections import defaultdict
    from itertools import repeat
    from typing import DefaultDict

    import rich
    import rich.traceback
    from rich.color import ANSI_COLOR_NAMES
    from rich.console import Console
    from rich.table import Table

    rich.traceback.install(show_locals=True)

    console = Console()

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

    # def show_sea_progress(
    #     sea: Sea,
    #     islands: List[Island],
    #     island_colors: Dict[int, Color],
    #     width: int,
    #     current_row_index: int,
    # ) -> None:
    #     point_colors: Dict[Coordinate, Color] = {}
    #     for island_index, island in enumerate(islands):
    #         color = island_colors[island_index]
    #         point_colors.update((coordinate, color) for coordinate in island)
    #     for row_index, row in enumerate(sea):
    #         for column_index, value in enumerate(row):
    #             coordinate = Coordinate(row_index, column_index)
    #             color = point_colors.get(coordinate, Color("black"))
    #             rich.print(f"[white on {color}] [/white on {color}]", end="")

    #         if current_row_index == row_index:
    #             print("<")
    #         else:
    #             print("")

    #     print("-" * width)

    class IslandAndColor(NamedTuple):
        island: Island
        color: Color

    class SeaTable(Table):
        island_colors: List[IslandAndColor]

        @classmethod
        def make_grid(cls) -> "SeaTable":
            return cls(
                box=None,
                padding=0,
                collapse_padding=True,
                show_header=False,
                show_footer=False,
                show_edge=False,
                pad_edge=False,
                expand=False,
            )

        @classmethod
        def from_sea(cls, sea: Sea) -> "SeaTable":
            grid = cls.make_grid()
            width = len(sea[0])
            print(f"width -> {width}")
            height = len(sea)
            print(f"height -> {height}")

            for _ in range(width):
                grid.add_column()
            for _ in range(height):
                grid.add_row(*[" "] * width, style="white on black")

            grid.island_colors = []

            return grid

        def remove_duplicate_islands(self) -> None:
            unique_islands: Set[IslandAndColor] = set()
            duplicate_island_indeces: List[int] = []
            for island_index, island_and_color in enumerate(self.island_colors):
                old_len = len(unique_islands)
                unique_islands.add(island_and_color)
                if len(unique_islands) == old_len:
                    duplicate_island_indeces.append(island_index)

            for index in reversed(duplicate_island_indeces):
                self.island_colors.pop(index)

        def update_colors(self) -> None:
            coordinates_and_colors: DefaultDict[Coordinate, Color] = defaultdict(
                lambda: Color("black")
            )
            for island, color in self.island_colors:
                coordinates_and_colors.update(zip(island, repeat(color)))

            for column_index, column in enumerate(self.columns):
                for row_index, _ in enumerate(column._cells):
                    coordinate = Coordinate(row_index, column_index)
                    column._cells[
                        row_index
                    ] = f"[white on {coordinates_and_colors[coordinate]}] [/]"

        def update_islands(
            self, islands: Iterable[Island], random_color: Callable[[], Color]
        ) -> None:
            for new_island in islands:
                merge_island_indeces: List[int] = []
                for (island_index, (island, color)) in enumerate(self.island_colors):
                    if island.issubset(new_island):
                        self.island_colors[island_index] = IslandAndColor(
                            new_island, color
                        )
                else:
                    self.island_colors.append(
                        IslandAndColor(new_island, random_color())
                    )

            self.remove_duplicate_islands()

            self.update_colors()


def number_of_islands(sea: Sea) -> int:
    number_of_rows = len(sea)
    assert number_of_rows >= 1, "empty sea"
    width = len(sea[0])

    for row in sea:
        assert len(row) == width, "non-rectangular sea"

    islands: List[Island] = []

    if DEBUGGING:
        random_color = new_random_color_gen()
        # island_colors: Dict[int, Color] = {0: random_color()}
        sea_table = SeaTable.from_sea(sea)

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

                islands.append(Island({land}))

        if DEBUGGING:
            rich.print(islands)
            sea_table.update_islands(islands, random_color=random_color)
            rich.print(sea_table)
            print("-" * width)

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
        TestCase(
            [
                [1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0],
                [0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0],
                [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0],
                [1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
                [1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0],
                [0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1],
                [1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1],
                [0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0],
                [0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0],
            ],
            2,
        ),
    ]

    for test_number, test in enumerate(test_cases):

        header = f"test #{test_number + 1}"
        if DEBUGGING:
            console.rule(header)
        else:
            print(header)

        answer = number_of_islands(stringify(test.case))
        if answer != test.correct_answer:
            if DEBUGGING:
                pass
            else:
                print("[")
                for row in test.case:
                    print(f" {row}")
                print("]")
                print(f"incorrect: {answer}")
            sys.exit(1)

        sys.exit(0)

    message = "tests passed"
    if DEBUGGING:
        console.rule(message, style="bold green")
    else:
        print(message)


class Solution:
    def numIslands(self, grid: List[List[str]]) -> int:
        return number_of_islands(grid)
