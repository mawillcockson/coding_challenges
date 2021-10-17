# mypy: allow-any-expr, warn-unused-configs
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
# pylint: disable=redefined-outer-name,ungrouped-imports

import ast
import random
import sys
from itertools import chain, repeat
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple, NewType, Set, cast


class Coordinate(NamedTuple):
    "Cartesian coordinate"
    row: int
    column: int


Island = NewType("Island", Set[Coordinate])
Color = NewType("Color", str)


CONTEXT_MANAGER_TYPING_URL = (
    "https://adamj.eu/tech/2021/07/04/"
    "python-type-hints-how-to-type-a-context-manager/"
    "#class-based-context-managers"
)
HUGE_TEST_CASES = Path("huge_test_cases.txt")

RICH = False
DEBUGGING = RICH or False

if RICH or TYPE_CHECKING:
    from collections import defaultdict
    from time import sleep
    from types import TracebackType
    from typing import (
        Callable,
        ContextManager,
        DefaultDict,
        Iterable,
        List,
        Optional,
        Type,
        Union,
    )

    import rich
    import rich.traceback
    from rich.color import ANSI_COLOR_NAMES
    from rich.console import Console
    from rich.layout import Layout
    from rich.live import Live
    from rich.prompt import Prompt
    from rich.rule import Rule
    from rich.table import Table
    from rich.text import Text

    Sea = List[List[str]]

    class TestFailure(Exception):
        """
        unit test failure

        mainly to get rich's tracebacks
        """

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
        """
        make a random color function

        this can run out of colors
        """
        # iter() can be replaced with cycle(), but it's better to ensure unique
        # colors
        random_color_iter = iter(random_colors)

        def random_color() -> Color:
            "return next generator item"
            return next(random_color_iter)

        return random_color

    class IslandAndColor(NamedTuple):
        """
        collection of island and color

        unhashable because of Island -> Set[...]
        """

        island: Island
        color: Color

    class SeaTable(Table):
        "main class for pretty-printing the status of number_of_islands()"
        island_colors: List[IslandAndColor]
        live: Optional[Live]

        @classmethod
        def make_grid(cls) -> "SeaTable":
            """
            override for super().grid() so we can return our own class
            """
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
            "the main way this class should be constructed"
            grid = cls.make_grid()
            width = len(sea[0])
            # if DEBUGGING:
            # print(f"width -> {width}")
            height = len(sea)
            # if DEBUGGING:
            # print(f"height -> {height}")

            for _ in range(width):
                grid.add_column()
            for _ in range(height):
                grid.add_row(
                    *[Text(" ", style="white on black") for _ in range(width)],
                    style="white on black",
                )

            grid.island_colors = []
            grid.live = None

            return grid

        def remove_duplicate_islands(self) -> None:
            "find matching islands and only keep the first"
            unique_islands: List[Island] = []
            duplicate_island_indeces: List[int] = []
            for island_index, (island, _) in enumerate(self.island_colors):
                if island in unique_islands:
                    duplicate_island_indeces.append(island_index)
                unique_islands.append(island)

            for index in reversed(duplicate_island_indeces):
                self.island_colors.pop(index)

        def update_colors(self) -> None:
            "re-apply island colors to grid"
            coordinate_color: DefaultDict[Coordinate, Color] = defaultdict(
                lambda: Color("black")
            )
            for island, color in self.island_colors:
                coordinate_color.update(zip(island, repeat(color)))

            for column_index, column in enumerate(self.columns):
                # pylint: disable=protected-access
                for row_index, _ in enumerate(column._cells):
                    coordinate = Coordinate(row_index, column_index)
                    cell = cast(Text, column._cells[row_index])
                    column._cells[row_index] = Text(
                        cell.plain, style=f"white on {coordinate_color[coordinate]}"
                    )

        def update_islands(
            self, islands: Iterable[Island], random_color: Callable[[], Color]
        ) -> None:
            """
            replace existing islands with the new larger ones
            """
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
            "refresh Rich's output"
            if self.live:
                self.live.refresh()
                if self.live.refresh_per_second:
                    sleep(1 / self.live.refresh_per_second)
                else:
                    if Prompt.ask(""):
                        sys.exit(1)

        def considering(
            self, land: Coordinate, coordinate: Coordinate
        ) -> ContextManager[None]:
            # pylint: disable=no-self-use
            "indicate which cells are being considered, and reset"
            if land.row == coordinate.row:
                if land.column < coordinate.column:
                    land_character = ">"
                elif land.column > coordinate.column:
                    land_character = "<"
                else:
                    land_character = "="
            elif land.column == coordinate.column:
                if land.row < coordinate.row:
                    land_character = "↓"
                elif land.row > coordinate.row:
                    land_character = "^"
            else:
                land_character = "?"

            coordinate_character = "*"

            # self.live.stop()
            # rich.inspect(self)
            # sys.exit(0)

            width = len(self.columns)
            height = self.row_count

            # pylint: disable=protected-access
            if (
                0 <= coordinate.column <= width - 1
                and 0 <= coordinate.row <= height - 1
            ):
                coordinate_cell = cast(
                    Text, self.columns[coordinate.column]._cells[coordinate.row]
                )
                original_coordinate_text = coordinate_cell.plain
                coordinate_cell.plain = coordinate_character

            else:
                coordinate_cell = Text()
                original_coordinate_text = ""

            if 0 <= land.column <= width - 1 and 0 <= land.row <= height - 1:
                land_cell = cast(Text, self.columns[land.column]._cells[land.row])
                original_land_text = land_cell.plain
                land_cell.plain = land_character

            else:
                land_cell = Text()
                original_land_text = ""

            self.live_refresh()

            class TempContextManager:
                # From CONTEXT_MANAGER_TYPING_URL
                """
                only exists because I can't get mypy to like contextmanager
                """

                def __enter__(self) -> None:
                    "do nothing"

                def __exit__(
                    self,
                    exc_type: Optional[Type[BaseException]],
                    exc_val: Optional[BaseException],
                    exc_tb: Optional[TracebackType],
                ) -> None:
                    "reset cell text"
                    coordinate_cell.plain = original_coordinate_text
                    land_cell.plain = original_land_text

            return cast("ContextManager[None]", TempContextManager())


if not RICH or TYPE_CHECKING:
    from types import TracebackType
    from typing import Type

    class FakeLive:
        "dummy Live"

        def __enter__(self) -> None:
            "do nothing"

        def __exit__(
            self,
            exc_type: "Optional[Type[BaseException]]",
            exc_val: "Optional[BaseException]",
            exc_tb: "TracebackType",
        ) -> None:
            "do nothing"


def number_of_islands(sea: "Sea") -> int:
    # pylint: disable=too-many-locals,too-many-branches
    """count number of distinct groups of adjacent "1"'s"""
    number_of_rows = len(sea)
    assert number_of_rows >= 1, "empty sea"
    width = len(sea[0])

    for row in sea:
        assert len(row) == width, "non-rectangular sea"

    islands: List[Island] = []

    if RICH:
        random_color = new_random_color_gen()

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

                    if RICH:
                        with sea_table.considering(land, coordinate):
                            pass

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

            if RICH:
                sea_table.update_islands(islands, random_color=random_color)

    if RICH:
        sea_table.update_islands(islands, random_color=random_color)
        sleep(1)

    return len(islands)


if __name__ == "__main__":

    def stringify(ints: "Union[List[List[int]], Sea]") -> "Sea":
        "normalize test-case representations"
        return [
            [
                "1" if (isinstance(value, str) and value != "0") or value else "0"
                for value in row
            ]
            for row in ints
        ]

    class TestCase(NamedTuple):
        "test case and answer"
        case: "Union[List[List[int]], Sea]"
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
        TestCase(
            [
                ["1", "1", "1", "1", "0"],
                ["1", "1", "0", "1", "0"],
                ["1", "1", "0", "0", "0"],
                ["0", "0", "0", "0", "0"],
            ],
            1,
        ),
    ]

    if HUGE_TEST_CASES.is_file():
        huge_tests = ast.literal_eval(HUGE_TEST_CASES.read_text(encoding="utf-8"))

        # with HUGE_TEST_CASES.open(mode="wt") as file:
        #     file.write("[\n")
        #     for case, answer in huge_tests:
        #         file.write(" [\n")
        #         file.write("  [\n")
        #         for row in case:
        #             file.write("   ")
        #             file.write(repr(row))
        #             file.write(",\n")
        #         file.write("  ],\n")
        #         file.write(f"  {answer},\n")
        #         file.write(" ],\n")
        #     file.write("]")
        #     sys.exit(0)

        for case, answer in huge_tests:
            test_cases.append(TestCase(cast("Sea", case), int(answer)))

    for test_number, test in enumerate(test_cases):
        sea = stringify(test.case)
        header = f"test #{test_number + 1}"

        if TYPE_CHECKING:
            live: Union[FakeLive, Live]

        if RICH or TYPE_CHECKING:
            sea_table = SeaTable.from_sea(sea)
            layout = Layout()
            layout.split_column(
                Layout(Rule(header), name="header"),
                Layout(sea_table, name="table"),
                Layout("press Enter to continue", name="footer"),
            )
            layout["header"].size = 1
            layout["footer"].size = 1
            live = Live(layout, auto_refresh=False, refresh_per_second=16, screen=True)
        else:
            live = FakeLive()

        if not RICH and DEBUGGING:
            print(header)

        with live:

            if RICH:
                if isinstance(live, Live):
                    sea_table.live = live

            ANSWER = number_of_islands(sea)

        if ANSWER != test.correct_answer:
            # pylint: disable=no-else-raise
            if RICH:
                rich.print(sea_table)
                print(f"incorrect: {ANSWER}")
                raise TestFailure
            elif DEBUGGING:
                print("[")
                for row in test.case:
                    print(f" {row}")
                print("]")
                print(f"incorrect: {ANSWER}")
            sys.exit(1)

    MESSAGE = "tests passed"
    if RICH:
        console.rule(MESSAGE)
    elif DEBUGGING:
        print(MESSAGE)


class Solution:
    "required by leetcode"

    # pylint: disable=invalid-name,no-self-use,too-few-public-methods
    def numIslands(self, grid: "List[List[str]]") -> int:
        "required by leetcode"
        return number_of_islands(grid)
