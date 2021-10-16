from copy import copy
from itertools import chain
from typing import List, NamedTuple, NewType, Sequence, Set, TypeVar

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

    def stringify(ints: List[List[int]]) -> Sea:
        return [[str(value) for value in row] for row in ints]

    assert (
        number_of_islands(
            stringify(
                [
                    [0, 0, 0],
                    [0, 1, 0],
                    [0, 0, 0],
                ]
            )
        )
        == 1
    )


class Solution:
    def numIslands(self, grid: List[List[str]]) -> int:
        return number_of_islands(grid)
