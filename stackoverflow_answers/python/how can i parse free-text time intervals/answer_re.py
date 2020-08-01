"""
Example parsing date and time interval with re
"""
import re
from datetime import timedelta
from functools import reduce
from operator import add, mul
from typing import List, Tuple

__all__ = [
    "examples",
    "parse",
]

examples = list(
    filter(
        None,
        """
1 second
2 minutes
3 hours
4 days
5 weeks
6 months
7 years

1 month, 0.05 weeks
0.003y, 100000secs
3y 4mo 9min 6d
1mo,3d 1.3e2 hours,
0.04yrs 2miasdf
""".splitlines(),
    )
)


comma = ","
ws = r"\s"
separator = fr"[{ws}{comma}]+"


def unit_name(string: str) -> re.Pattern:
    return re.compile(fr"{string}\w*")


second = unit_name("s")
minute = unit_name("mi")
hour = unit_name("h")
day = unit_name("d")
week = unit_name("w")
month = unit_name("mo")
year = unit_name("y")
units = {
    second: timedelta(seconds=1),
    minute: timedelta(minutes=1),
    hour: timedelta(hours=1),
    day: timedelta(days=1),
    week: timedelta(weeks=1),
    month: timedelta(days=30),
    year: timedelta(days=365),
}
unit = re.compile(
    "("
    + "|".join(
        regex.pattern for regex in [second, minute, hour, day, week, month, year]
    )
    + ")"
)
digit = r"\d"
integer = fr"({digit}+)"
decimal = fr"({integer}\.({integer})?|\.{integer})"
signed_integer = fr"([+-]?{integer})"
exponent = fr"([eE]{signed_integer})"
float_ = fr"({integer}{exponent}|{decimal}({exponent})?)"
number = re.compile(fr"({float_}|{integer})")
time = re.compile(fr"(?P<number>{number.pattern}){ws}*(?P<unit>{unit.pattern})")
interval = re.compile(fr"({time.pattern}({separator})*)+", flags=re.IGNORECASE)


def normalize_unit(text: str) -> timedelta:
    "maps units to their respective timedelta"
    if not unit.match(text):
        raise ValueError(f"Not a unit: {text}")

    for unit_re in units:
        if unit_re.match(text):
            return units[unit_re]

    raise ValueError(f"No matching unit found: {text}")


def parse(text: str) -> timedelta:
    if not interval.match(text):
        raise ValueError(f"Parser Error: {text}")

    parsed_pairs: List[Tuple[float, timedelta]] = list()
    for match in time.finditer(text):
        parsed_number = float(match["number"])
        parsed_unit = normalize_unit(match["unit"])
        parsed_pairs.append((parsed_number, parsed_unit))

    timedeltas = [mul(*pair) for pair in parsed_pairs]

    return reduce(add, timedeltas, timedelta(seconds=0))


if __name__ == "__main__":
    parsed_examples = [(example, parse(example)) for example in examples]
    longest_example = max(map(lambda tup: len(tup[0]), parsed_examples))
    longest_formatted = max(map(lambda tup: len(f"{tup[1]!s}"), parsed_examples))
    longest_parsed = max(map(lambda tup: len(f"<{tup[1]!r}>"), parsed_examples))
    for example, parsed_example in parsed_examples:
        print(
            f"{example: <{longest_example}s} -> "
            f"{parsed_example!s: <{longest_formatted}s} "
            f"{'<' + repr(parsed_example) + '>': >{longest_parsed}s}"
        )
