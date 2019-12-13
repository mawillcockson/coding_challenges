import sys
from pathlib import Path
from typing import Generator, Tuple, Union, List
from collections import namedtuple
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

first_wire = list(map(string_to_vector, wire_paths[0]))
second_wire = list(map(string_to_vector, wire_paths[1]))
