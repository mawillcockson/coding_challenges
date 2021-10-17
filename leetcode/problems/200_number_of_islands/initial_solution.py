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
    from rich.layout import Layout
    from rich.live import Live
    from rich.pretty import pprint
    from rich.prompt import Prompt
    from rich.rule import Rule
    from rich.table import Table
    from rich.text import Text

    class TestFailure(Exception):
        pass

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

    class IslandAndColor(NamedTuple):
        island: Island
        color: Color

    class SeaTable(Table):
        island_colors: List[IslandAndColor]
        live: Optional[Live]

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
            # print(f"width -> {width}")
            height = len(sea)
            # print(f"height -> {height}")

            for _ in range(width):
                grid.add_column()
            for _ in range(height):
                grid.add_row(*[" "] * width, style="white on black")

            grid.island_colors = []

            return grid

        def remove_duplicate_islands(self) -> None:
            unique_islands: List[Island] = []
            duplicate_island_indeces: List[int] = []
            for island_index, (island, color) in enumerate(self.island_colors):
                if island in unique_islands:
                    duplicate_island_indeces.append(island_index)
                unique_islands.append(island)

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
                    column._cells[row_index] = Text(
                        " ", style=f"white on {coordinates_and_colors[coordinate]}"
                    )

        def update_islands(
            self, islands: Iterable[Island], random_color: Callable[[], Color]
        ) -> None:
            for new_island in islands:
                subsset = False
                for (island_index, (island, color)) in enumerate(self.island_colors):
                    if island.issubset(new_island):
                        self.island_colors[island_index] = IslandAndColor(
                            new_island, color
                        )
                        subsset = True

                if not subsset:
                    self.island_colors.append(
                        IslandAndColor(new_island, random_color())
                    )

            self.remove_duplicate_islands()

            self.update_colors()

            self.live_refresh()

        def live_refresh(self) -> None:
            if self.live:
                self.live.refresh()
                Prompt.ask("")

        def considering(self, land: Coordinate, coordinate: Coordinate) -> None:
            if land.row == coordinate.row:
                if land.column < coordinate.column:
                    land_character = ">"
                elif land.column > coordinate.column:
                    land_character = "<"
                else:
                    land_character = "="
            elif land.column == coordinate.column:
                if land.row > coordinate.row:
                    land_character = "↓"
                elif land.row < coordinate.row:
                    land_character = "^"
            else:
                land_character = "?"

            coordinate_character = "*"

            # self.live.stop()
            # rich.inspect(self)
            # sys.exit(0)

            width = len(self.columns)
            height = self.row_count

            # coordinate_cell = self.columns[coordinate.column]._cells[coordinate.row]
            # self.columns[coordinate.column]._cells[coordinate.row] = Text(
            #     coordinate_character
            # )
            # self.columns[coordinate.column]._cells[coordinate.row].copy_style(
            #     coordinate_cell
            # )
            if (
                0 <= coordinate.column <= width - 1
                and 0 <= coordinate.row <= height - 1
            ):
                self.columns[coordinate.column]._cells[
                    coordinate.row
                ] = coordinate_character

            # land_cell = self.columns[land.column]._cells[land.row]
            # self.columns[land.column]._cells[land.row] = Text(land_character)
            # self.columns[land.column]._cells[land.row].copy_style(land_cell)
            if 0 <= land.column <= width - 1 and 0 <= land.row <= height - 1:
                self.columns[land.column]._cells[land.row] = land_character

            self.live_refresh()


if not DEBUGGING or TYPE_CHECKING:
    from contextlib import AbstractContextManager

    class FakeLive(AbstractContextManager[None]):
        def __enter__(self) -> None:
            return None


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
                for coordinate in [left_coordinate, right_coordinate, above_coordinate]:

                    if DEBUGGING:
                        sea_table.considering(land, coordinate)

                    if coordinate in island:
                        adjacent_island_indeces.append(island_index)
                        break

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
        sea_table.update_islands(islands, random_color=random_color)

    return len(islands)


if __name__ == "__main__":

    def stringify(ints: Union[List[List[int]], Sea]) -> Sea:
        return [["1" if value else "0" for value in row] for row in ints]

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
            3,
        ),
        TestCase(
            [
                [1, 0, 1, 0, 1],
                [1, 0, 1, 0, 1],
                [1, 0, 1, 0, 1],
                [1, 0, 1, 0, 1],
                [1, 0, 1, 0, 1],
                [1, 1, 1, 1, 1],
            ],
            1,
        ),
    ]

    for test_number, test in enumerate(test_cases):
        sea = stringify(test.case)
        header = f"test #{test_number + 1}"

        if TYPE_CHECKING:
            live: Union[FakeLive, Live]

        if DEBUGGING or TYPE_CHECKING:
            sea_table = SeaTable.from_sea(sea)
            layout = Layout()
            layout.split_column(
                Layout(Rule(header), name="header"),
                Layout(sea_table, name="table"),
                Layout("press Enter to continue", name="footer"),
            )
            layout["header"].size = 1
            layout["footer"].size = 1
            live = Live(layout, auto_refresh=False, screen=True)
        else:
            live = FakeLive()

        if not DEBUGGING:
            print(header)

        with live:

            if DEBUGGING:
                if isinstance(live, Live):
                    sea_table.live = live

            answer = number_of_islands(sea)

        if answer != test.correct_answer:
            if DEBUGGING:
                raise TestFailure
            else:
                print("[")
                for row in test.case:
                    print(f" {row}")
                print("]")
                print(f"incorrect: {answer}")
            sys.exit(1)

    message = "tests passed"
    if DEBUGGING:
        console.rule(message)
    else:
        print(message)


class Solution:
    def numIslands(self, grid: List[List[str]]) -> int:
        return number_of_islands(grid)
