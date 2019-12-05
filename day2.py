from operator import add, mul
from pathlib import Path
from shutil import get_terminal_size
from typing import Sequence, Tuple, Union

from prompt_toolkit import PromptSession
from prompt_toolkit.application import Application
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.layout.containers import (
    Float,
    FloatContainer,
    HSplit,
    VSplit,
    Window,
    WindowAlign
)
from prompt_toolkit.layout.controls import FormattedTextControl
from prompt_toolkit.layout.dimension import Dimension
from prompt_toolkit.layout.layout import Layout
from prompt_toolkit.widgets.base import Frame

# We have an input tape with all the opcodes and input in a file
# We read it in, and turn it into a list of integers
tape = list(
    map(
        int,
        Path("day2_input.txt").read_text().split(","),
    )
)
# We also want a pointer that keeps track of which opcode we're executing
pointer: int = 0

# Part of the problem statement described modifying the tape
tape[1] = 12
tape[2] = 2

def op(opcode: int, val1: int, val2: int, output: int) -> None:
    if not isinstance(opcode, int) or opcode not in [1,2,99]:
        raise NotImplementedError("Bad opcode!")
    if opcode == 99:
        raise StopIteration("STOP")
    
    if opcode == 1:
        tape[output] = add(val1, val2)
    else:
        tape[output] = mul(val1, val2)

def current_instruction() -> Tuple[int, int, int, int]:
    return tuple(tape[pointer, pointer + 4])

def behind_tape() -> Sequence[int]:
    return tape[:pointer]

def ahead_tape() -> Sequence[int]:
    return tape[pointer + 4:]

bindings = KeyBindings()
@bindings.add("c-c")
def _(event):
    event.app.exit()

@bindings.add("left")
def _(event):
    "Move the tape left"

@bindings.add("right")
def _(event):
    "Move the tape right"

app = Application(
    layout=Layout(
        HSplit([
            FloatContainer(
                content=Window(content=FormattedTextControl(text=""), height=1),
                floats=[
                    Float(
                        content=Window(char="┌"),
                        top=0,
                        left=36,
                        width=1,
                        height=1,
                    ),
                    Float(
                        content=Window(char="─"),
                        width=7,
                        height=1,
                        top=0,
                        left=37,
                    ),
                    Float(
                        content=Window(char="┐"),
                        width=1,
                        height=1,
                        top=0,
                        left=44,
                    )
                ],
            ),
            FloatContainer(
                content=HSplit([
                    Window(content=FormattedTextControl(text=""), height=1),
                    Window(
                        content=FormattedTextControl(text="    0       1       3       4       5       6       7       8       9      10      11"),
                        height=1,
                        align=WindowAlign.LEFT,
                    ),
                    Window(content=FormattedTextControl(text=""), height=1),
                ]),
                floats=[
                    Float(
                        content=Frame(
                            body=Window(content=FormattedTextControl(text="")),
                        ),
                        height=3,
                        width=32,
                        left=7,
                        transparent=True,
                    ),
                    Float(
                        content=Window(content=FormattedTextControl(text="↓")),
                        width=1,
                        height=1,
                        left=44,
                        top=0,
                    )
                ]
            )
        ],
        )
    ),
    key_bindings=bindings,
)
if __name__ == "__main__":
    app.run()


#app = Application(
#    layout=Layout(
#        VSplit(
#            children=[
#                HSplit(
#                    children=[
#                        Window(
#                            content=FormattedTextControl(text="      "),
#                            height=1,
#                            width=6,
#                        ),
#                        Window(
#                            content=FormattedTextControl(text="    0 "),
#                            height=1,
#                        ),
#                        Window(
#                            content=FormattedTextControl(text="      "),
#                            height=1,
#                        )
#                    ],
#                    width=6,
#                ),
#                HSplit(
#                    children=[
#                        Window(char="┏", height=1, width=1),
#                        Window(char="┃", height=1, width=1),
#                        Window(char="┗", height=1, width=1)
#                    ]
#                ),
#                HSplit(
#                    children=[
#                        Window(char="━", width=31, height=1),
#                        Window(
#                            content=FormattedTextControl(
#                                text="     1       3       4       5 "
#                            ),
#                            height=1,
#                        ),
#                        Window(char="━", width=31, height=1),
#                    ]
#                )
#            ]
#        )
#    ),
#    key_bindings=bindings,
#)
#app.run()

#app = Application(
#    layout=Layout(
#        HSplit(
#            [
#                VSplit([
#                    Window(char='┌', width=36, height=1),
#                    Window(char='─', width=7, height=1),
#                    Window(char='┐', width=1, height=1),
#                ]),
#                VSplit([
#                    Window(content=FormattedTextControl(text="    0 "), width=6, height=3, align=WindowAlign.CENTER),
#                    Frame(
#                        body=Window(content=FormattedTextControl(
#                            text="     1       3       4       5 "
#                        ),
#                        width=32,
#                        height=3,
#                    )),
#                    HSplit([
#                        Window(content=FormattedTextControl(text="     ↓"), height=1),
#                        Window(
#                            content=FormattedTextControl(
#                                text="     6       7       8       9"
#                            ),
#                            height=1,
#                        ),
#                        Window(char=" ", height=1)
#                    ])
#                ]),
#                Window(content=FormattedTextControl(text="           ↓       ↓       ↓")),
#                Window(content=FormattedTextControl(text="         add        3       4 = 7")),
#            ],
#            height=Dimension(min=6,max=6),
#        )
#    ),
#    min_redraw_interval=0.5,
#    key_bindings=bindings,
#)
#app.run()
