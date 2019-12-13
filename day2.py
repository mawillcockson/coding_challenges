import sys
from pathlib import Path
import inspect
from typing import Sequence, Tuple, Union

# We have an input tape with all the opcodes and input in a file
# We read it in, and turn it into a list of integers
tape = list(
    map(
        int,
        Path("day2_input.txt").read_text().split(","),
    )
)

# Part of the problem statement described modifying the tape
tape[1] = 12
tape[2] = 2

#opcode_functions = {
#    1: lambda val1, val2, out: tape[out] = tape[val1] + tape[val2],
#    2: lambda val1, val2, out: tape[out] = tape[val1] * tape[val2],
#    99: lambda: raise StopIteration("STOP"),
#}

def op1(val1, val2, out):
    print(f"+ {(v1 := tape[val1]): >7} {(v2 := tape[val2]): >7} = {(res := v1 + v2): >7} -> {out}")
    tape[out] = res

def op2(val1, val2, out):
    print(f"* {(v1 := tape[val1]): >7} {(v2 := tape[val2]): >7} = {(res := v1 * v2): >7} -> {out}")
    tape[out] = res

opcode_functions = {
    1: op1,
    2: op2,
    99: lambda: StopIteration(f"STOP: {tape[0]}"),
}

instruction_lengths = {}

for opcode, func in opcode_functions.items():
    number_of_parameters = len(inspect.signature(func).parameters)
    instruction_lengths[opcode] = number_of_parameters + 1

def instructions() -> Sequence[int]:
    pointer: int = 0

    def gen():
        nonlocal pointer
        while pointer + 1 < len(tape):
            opcode = tape[pointer]
            if not (opcode in instruction_lengths):
                raise ValueError(f"Bad opcode: {opcode} @ {pointer}\n{tape}")
            instruction_length = instruction_lengths[opcode]
            yield tape[pointer : (pointer + instruction_length)]
            pointer += instruction_length

    yield from gen()

def op(instruction: Sequence[int]) -> None:
    opcode = instruction[0]
    instruction_parameters = instruction[1:]
    function = opcode_functions[opcode]
    result = function(*instruction_parameters)
    if isinstance(result, Exception):
        raise result
    else:
        return result

for instruction in instructions():
    op(instruction)
