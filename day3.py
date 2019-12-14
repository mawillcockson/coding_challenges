import sys
from pathlib import Path
from typing import Generator, Tuple, Union, List, Sequence
from collections import namedtuple
from itertools import cycle
import re

wire_paths = list(
    map(
        lambda s: s.split(","),
        Path("day3_input.txt").read_text().split(),
    )
)

Vector = namedtuple("Vector", "direction, distance")

vector_re = re.compile(r"^(?P<direction>[UDLR])(?P<distance>[0-9]+)$")
def string_to_vector(string: str) -> Vector:
    match = vector_re.match(string)
    if not match:
        raise ValueError(f"Can't understand vector: {string}")
    return Vector(match.group("direction"), int(match.group("distance")))

Point: Tuple[int, int]
Point = namedtuple("Point", "x, y")
def vectors_to_points(vecs: Sequence[Vector]) -> Generator[Point, None, None]:
    previous_point = Point(0,0)
    for vec in vecs:
        if vec.direction == "U":
            point = Point(
                previous_point.x,
                previous_point.y + vec.distance,
            )
        elif vec.direction == "D":
            point = Point(
                previous_point.x,
                previous_point.y - vec.distance,
            )
        elif vec.direction == "L":
            point = Point(
                previous_point.x - vec.distance,
                previous_point.y,
            )
        elif vec.direction == "R":
            point = Point(
                previous_point.x + vec.distance,
                previous_point.y,
            )
        yield point
        previous_point = point

repeat = lambda x: cycle([x])
def fill_coordinates(points: Sequence[Point]) -> Generator[Point, None, None]:
    previous_point = Point(0,0)
    for point in points:
        if point.x > previous_point.x:
            yield from zip(
                range(previous_point.x, point.x + 1),
                repeat(point.y),
            )
        elif point.x < previous_point.x:
            yield from zip(
                range(point.x, previous_point.x + 1),
                repeat(point.y),
            )
        elif point.y > previous_point.y:
            yield from zip(
                repeat(point.x),
                range(previous_point.y, point.y + 1),
            )
        elif point.y < previous_point.y:
            yield from zip(
                repeat(point.x),
                range(point.y, previous_point.y + 1),
            )
        previous_point = point

first_wire = map(string_to_vector, wire_paths[0])
first_wire = vectors_to_points(first_wire)
first_wire = fill_coordinates(first_wire)
first_wire = set(first_wire) - {(0,0)}

second_wire = map(string_to_vector, wire_paths[1])
second_wire = vectors_to_points(second_wire)
second_wire = fill_coordinates(second_wire)
second_wire = set(second_wire) - {(0,0)}

common_points = first_wire & second_wire

def manhattan_from_origin(point: Point) -> int:
    return abs(point[0]) + abs(point[1])

print(f"{min(map(manhattan_from_origin, common_points))}")