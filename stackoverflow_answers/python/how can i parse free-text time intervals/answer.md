## Original answer

**Not a solution because [`dateutil`] can parse points in time, but not intervals**

<s>
[`dateutil`] now supports all of the original requested intervals:

```python
from dateutil.parser import parse

examples = """
August 3rd, 2019
2019-08-03
2019, 3rd aug, 2:45 pm
"""

formatted_examples = [
    (example, f"{(p := parse(example))} <{p!r}>")
    for example in filter(None, examples.splitlines())
]
longest_example = max(map(lambda tup: len(tup[0]), formatted_examples))
longest_parsed = max(map(lambda tup: len(tup[1]), formatted_examples))

for example, parsed_example in formatted_examples:
    print(f"{example: <{longest_example}s} -> {parsed_example: >{longest_parsed}s}")
```

On PyPI, the package is called [`python-dateutil`].
</s>

## Parsing

We can write a parser. It doesn't make a huge difference which parser is used. I searched for "**python parser**" and chose [`lark`] because it popped up in the top of the results.

First, I defined the units as a mapping. This is where more units could be added, if "centuries" or "microseconds" are needed.

_Note: For very small or large numbers, keep in mind [`timedelta.resolution`]_

```python
units = {
    "second": timedelta(seconds=1),
    "minute": timedelta(minutes=1),
    "hour":   timedelta(hours=1),
    "day":    timedelta(days=1),
    "week":   timedelta(weeks=1),
    "month":  timedelta(days=30),
    "year":   timedelta(days=365),
}
```

Next, the grammar is defined using [`lark`'s variant of EBNF]. Here, `WS` hopefully matches all whitespace:

```python
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
```

The grammar should allow arbitrary time intervals to be chained together, with or without commas as separators.

Each time interval's unit can be given as the shortest unique prefix:

```text
second -> s
minute -> mi
hour   -> h
day    -> d
week   -> w
month  -> mo
year   -> y
```

Including the ones in the original question, these will serve as the target examples we want to parse:

```text
1 second
2 minutes
3 hours
4 days
5 weeks
6 months
7 years

1 month, 7 years, 2 days, 30 hours, 0.05 seconds
0.0003 years, 100000 seconds
3y 4mo 9min 6d
1mo,3d 1.3e2 hours, 0.04yrs 2mi444
```

Lastly, I followed [one of the `lark` tutorials] and used a [transformer]:

```python
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
```

The grammar is interpreted by `lark.Lark`.  Since it is compatible with
[`lark`'s LALR(1) parser], that parser is specified to gain some speed and
improve memory efficiency by allowing the transformer to be used directly by
the parser:

```python
time_interval_parser = Lark(
    grammar=time_interval_grammar,
    start="interval",
    parser="lalr",
    transformer=IntervalToTimedelta,
)
```

This produces a mostly working parser. The complete `answer.py` file is this:

```python
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

```

Running it runs through the examples:

```text
$ python .\answer.py
1 second            -> 0:00:01                         <datetime.timedelta(seconds=1)>
2 minutes           -> 0:02:00                       <datetime.timedelta(seconds=120)>
3 hours             -> 3:00:00                     <datetime.timedelta(seconds=10800)>
4 days              -> 4 days, 0:00:00                    <datetime.timedelta(days=4)>
5 weeks             -> 35 days, 0:00:00                  <datetime.timedelta(days=35)>
6 months            -> 180 days, 0:00:00                <datetime.timedelta(days=180)>
7 years             -> 2555 days, 0:00:00              <datetime.timedelta(days=2555)>
1 month, 0.05 weeks -> 30 days, 8:24:00   <datetime.timedelta(days=30, seconds=30240)>
0.003y, 100000secs  -> 2 days, 6:03:28     <datetime.timedelta(days=2, seconds=21808)>
3y 4mo 9min 6d      -> 1221 days, 0:09:00 <datetime.timedelta(days=1221, seconds=540)>
1mo,3d 1.3e2 hours, -> 38 days, 10:00:00  <datetime.timedelta(days=38, seconds=36000)>
0.04yrs 2miasdf     -> 14 days, 14:26:00  <datetime.timedelta(days=14, seconds=51960)>
```

This works fine, and the performance is adequate:

```shell
$ python -m timeit -s "from answer import parse, examples" "for example in examples:" " parse(example)"
500 loops, best of 5: 415 usec per loop
```

### Potential improvements

Currently, this does not have any error handling, though this is by ommission:
`lark` does raise errors, so the `parse()` function could catch any that can be
handled gracefully.

Some other downsides to this particular implementation:

- Doesn't type check with [`mypy --strict`]
- It requires the use of a 3rd-party library
- The grammar could better [shape the resulting parse tree]

## Regular Expressions

Alternatively, instead of using a library for parsing, regular expressions can be used with the builtin [`re`].

This has a few disadvantages:

- Regular expressions are challenging to make flexible
- Complex regular expressions are difficult to read
- Regular expressions generally take longer for a human to interpret

It can be faster, though, and should only need the standard library included in [CPython].

Using the previous example as a starting point, this is one way regular expressions could be swapped in:

```python
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
```

The number parsing is mimicked from [`lark`'s builtin grammar definitions].

The performance for this is better:

```text
$ python -m timeit -s "from answer_re import parse, examples" "for example in examples:" " parse(example)"
2000 loops, best of 5: 109 usec per loop
```

But it's less readable, and making changes to maintain it will require more work.

## Notes

As-is, both examples behave in a way that doesn't quite match up with how humans expect time intervals to work:

```python
>>> from answer_re import parse
>>> from datetime import datetime
>>> datetime(2000, 1, 1) + parse("9 years")
datetime.datetime(2008, 12, 29, 0, 0)
>>> str(_)
'2008-12-29 00:00:00'
```

Compare this to what most people would expect it to be:

[![9 years from 2000-01-01 results in 2009-01-01][wolframalpha image]][wolframalpha link]

[This stack overflow question provides a few solutions][other], one of which uses [`dateutil`]. Both of the examples above can be adapted by modifying the `units` mapping to use appropriate [`relativedelta`'s].

This is what the first example would look like:

```python
...

units = {
    "second": relativedelta(seconds=1),
    "minute": relativedelta(minutes=1),
    "hour": relativedelta(hours=1),
    "day": relativedelta(days=1),
    "week": relativedelta(weeks=1),
    "month": relativedelta(months=1),
    "year": relativedelta(years=1),
}

...
```

This returns what's expected:

```python
>>> from answer_with_dateutil import parse
>>> from datetime import datetime
>>> datetime(2000, 1, 1) + parse("9 years")
datetime.datetime(2009, 1, 1, 0, 0)
>>> str(_)
'2009-01-01 00:00:00'
```

Also, the use of [f-strings] and [type annotations] restricts this to Python 3.6 and up, though this can be changed to use [`str.format`] instead for Python 3.5+.

## Conclusion

With [the currently accepted answer] in the running, this is the performance for the more normal examples given in the original question:

_Note: for `sh`, replace `` ` `` with `\` in the following commands_

```powershell
$ python -m timeit -s "from answer import examples;examples = examples[:7]" `
    -s "from parsedatetime import Calendar; from datetime import datetime" `
    -s "parse = Calendar().parseDT; now = datetime.now()" `
    "for example in examples:" " parse(example)[0] - now"
1000 loops, best of 5: 232 usec per loop

$ python -m timeit -s "from answer_re import examples;examples = examples[:7]" `
    -s "from answer import parse" `
    "for example in examples:" " parse(example)"
2000 loops, best of 5: 157 usec per loop

$ python -m timeit -s "from answer_re import examples;examples = examples[:7]" `
    -s "from answer_re import parse" `
    "for example in examples:" " parse(example)"
10000 loops, best of 5: 39.5 usec per loop
```

The performance differences are largely negligible for a large variety of use cases.

Currently, the easiest one to use is going to be the example given in [the currently accepted answer]:

Unless very custom parsing is needed, use [`parsedatetime`].

[`dateutil`]: <https://dateutil.readthedocs.io/en/stable/parser.html> "dateutil.parser module documentation"
[`python-dateutil`]: <https://pypi.org/project/python-dateutil/> "dateutil library on the Python Package Index"
[`lark`]: <https://lark-parser.readthedocs.io/en/latest/> "lark library on PyPI"
[`re`]: <https://docs.python.org/3/library/re.html> "re library in Python's docs"
[wolframalpha image]: <https://i.imgur.com/Wu42bVT.png>
[wolframalpha link]: <https://www.wolframalpha.com/input/?i=9+years+from+2000-01-01> "Link to the same query on WolframAlpha"
[other]: <https://stackoverflow.com/q/546321/5059062> "How do I calculate the date six months from the current date using the datetime Python module?"
[`timedelta.resolution`]: <https://docs.python.org/3/library/datetime.html#datetime.timedelta.resolution> "datetime.timedelta.resolution in Python's docs"
[`mypy --strict`]: <https://mypy.readthedocs.io/en/stable/command_line.html#cmdoption-mypy-strict> "--strict flag in mypy's docs"
[`lark`'s variant of EBNF]: <https://lark-parser.readthedocs.io/en/latest/grammar/#grammar-reference> "lark's grammar reference"
[`lark`'s LALR(1) parser]: <https://lark-parser.readthedocs.io/en/latest/parsers/#lalr1> "Description of lark's LALR(1) parser in lark's docs"
[f-strings]: <https://docs.python.org/3/reference/lexical_analysis.html#formatted-string-literals> "Formatted string literal syntax in Python's docs"
[type annotations]: <https://docs.python.org/3/whatsnew/3.6.html#pep-526-syntax-for-variable-annotations> "Type annotations introduced in Python's What's New doc"
[`str.format`]: <https://docs.python.org/3/library/stdtypes.html#str.format> "str.format in the Python docs"
[CPython]: <https://docs.python.org/3/reference/introduction.html#alternate-implementations> "List of Python implementations"
[`lark`'s builtin grammar definitions]: <https://github.com/lark-parser/lark/blob/d2f55fe3ba7b4bbc95ecdce2c06347cf2314ca4e/lark/grammars/common.lark#L1-L17> "lark's internal number parsing grammar definitions"
[`relativedelta`'s]: <https://dateutil.readthedocs.io/en/stable/relativedelta.html#module-dateutil.relativedelta> "reltaivedelta type in dateutil's docs"
[`parsedatetime`]: <https://github.com/bear/parsedatetime> "parsedatetime library on GitHub"
[one of the `lark` tutorials]: <https://github.com/lark-parser/lark/blob/7a13fb0f5b968046795fa9d221a38c2a34503605/docs/json_tutorial.md> "lark JSON parser tutorial"
[transformer]: <https://lark-parser.readthedocs.io/en/latest/visitors/#transformers> "Transformers explained in the lark docs"
[shape the resulting parse tree]: <https://lark-parser.readthedocs.io/en/latest/tree_construction/#shaping-the-tree> "Shaping the Tre in lark's docs"
[the currently accepted answer]: <https://stackoverflow.com/a/9775804/5059062> "The currently accepted answer"
