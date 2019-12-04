from typing import Tuple, Union
from pathlib import Path
from operator import add, mul
from shutil import get_terminal_size
from prompt_toolkit import PromptSession
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.application import Application
from prompt_toolkit.layout.layout import Layout
from prompt_toolkit.layout.containers import (
    HSplit,
    VSplit,
    Window,
    WindowAlign,
)
from prompt_toolkit.layout.controls import FormattedTextControl
from prompt_toolkit.layout.dimension import Dimension
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


bindings = KeyBindings()
@bindings.add('c-c')
def _(event):
    event.app.exit()

app = Application(
    layout=Layout(
        HSplit(
            [
                VSplit([
                    Window(char='┌', width=36, height=1),
                    Window(char='─', width=7, height=1),
                    Window(char='┐', width=1, height=1),
                ]),
                VSplit([
                    Window(content=FormattedTextControl(text="    0 "), width=6, height=3, align=WindowAlign.CENTER),
                    Frame(
                        body=Window(content=FormattedTextControl(
                            text="     1       3       4       5 "
                        ),
                        width=32,
                        height=3,
                    )),
                    HSplit([
                        Window(content=FormattedTextControl(text="     ↓"), height=1),
                        Window(
                            content=FormattedTextControl(
                                text="     6       7       8       9"
                            ),
                            height=1,
                        ),
                        Window(char=" ", height=1)
                    ])
                ]),
                Window(content=FormattedTextControl(text="           ↓       ↓       ↓")),
                Window(content=FormattedTextControl(text="         add        3       4 = 7")),
            ],
            height=Dimension(min=6,max=6),
        )
    ),
    min_redraw_interval=0.5,
    key_bindings=bindings,
)
app.run()