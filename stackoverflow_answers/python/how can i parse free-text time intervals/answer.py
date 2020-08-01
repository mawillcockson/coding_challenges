"""
Example parsing date and time interval with lark
"""
from datetime import timedelta
from functools import reduce
from operator import add, mul
from typing import List, Union

from lark import Lark, Token, Transformer

__all__ = [
    "examples",
    "IntervalToTimedelta",
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

units = {
    "second": timedelta(seconds=1),
    "minute": timedelta(minutes=1),
    "hour": timedelta(hours=1),
    "day": timedelta(days=1),
    "week": timedelta(weeks=1),
    "month": timedelta(days=30),
    "year": timedelta(days=365),
}

time_interval_grammar = r"""
%import common.WS
%import common.NUMBER

?interval: time+
time: value unit _separator?
value: NUMBER -> number
unit: SECOND
    | MINUTE
    | HOUR
    | DAY
    | WEEK
    | MONTH
    | YEAR
_separator: (WS | ",")+

SECOND: /s\w*/i
MINUTE: /mi\w*/i
HOUR:   /h\w*/i
DAY:    /d\w*/i
WEEK:   /w\w*/i
MONTH:  /mo\w*/i
YEAR:   /y\w*/i

%ignore WS
%ignore ","
"""


class IntervalToTimedelta(Transformer):
    def interval(tree: List[timedelta]) -> timedelta:
        "sums all timedeltas"
        return reduce(add, tree, timedelta(seconds=0))

    def time(tree: List[Union[float, timedelta]]) -> timedelta:
        "returns a timedelta representing the "
        return mul(*tree)

    def unit(tokens: List[Token]) -> timedelta:
        """
        converts a unit into a timedelta that represents 1 of the unit type
        """
        return units[tokens[0].type.lower()]

    def number(tokens: List[Token]) -> float:
        "returns the value as a python type"
        return float(tokens[0].value)


time_interval_parser = Lark(
    grammar=time_interval_grammar,
    start="interval",
    parser="lalr",
    transformer=IntervalToTimedelta,
)

parse = time_interval_parser.parse


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
