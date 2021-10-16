import sys
from pathlib import Path
import inspect
from typing import Generator, Tuple, Union, List

# We have an input tape that lists out the memory in a file
# We read it in, and turn it into a list of integers
tape: List[int]
tape = list(
    map(
        int,
        Path("day2_input.txt").read_text().split(","),
    )
)
# This tape is now a representation of our memory
memory = tape

# Part of the problem statement described modifying the memory
memory[1] = 12
memory[2] = 2

# There are various opcodes that perform various operations with the memory
# So far, there are three major ones, and they are implemented in these functions
def op1(arg1: int, arg2: int, out: int) -> None:
    '''opcode 1 -> adds together the values found in memory locations pointed to by
       arg1 and arg2, stores the result in the memory location pointed to by out
    '''
    val1 = memory[arg1]
    val2 = memory[arg2]
    result = val1 + val2
    memory[out] = result
    print(f"+ {val1: >7} {val2: >7} = {result: >7} -> {out: >7}")

def op2(arg1: int, arg2: int, out: int) -> None:
    '''opcode 1 -> multiplies together the values found in memory, using the first two
       arguments as addresses, and stores the result in memory at the adress pointed to
       by the third argument
    '''
    val1 = memory[arg1]
    val2 = memory[arg2]
    result = val1 * val2
    memory[out] = result
    print(f"* {val1: >7} {val2: >7} = {result: >7} -> {out: >7}")

# The STOP opcode is simple, and an exception to stop the machine
# In the problem statement, the memory position 0 holds the answer,
# so we return that with the Exception
def op99() -> None:
    raise StopIteration(f"STOP: {memory[0]}")

# All the opcode functions are stored in a table, indexed by their opcode
opcode_functions = {
    1: op1,
    2: op2,
    99: op99,
}

# It would be handy to have a table that lists how many parameters an opcode needs to form
# a full instruction, so we look at each function, count how many parameters it has,
# and add 1 to get the length of the instruction, and store that in a table indexed
# by the opcode
instruction_lengths = {}
for opcode, func in opcode_functions.items():
    number_of_parameters = len(inspect.signature(func).parameters)
    instruction_lengths[opcode] = number_of_parameters + 1

# This is a generator that takes the current value in memory at a position, starting at 0,
# and returns that opcode along with its arguments from memory.
# Because it has to grab the arguments for an opcode, it also happens to check if the current
# address is a valid opcode.
# Lastly, it increments the instruction pointer by the length of the total instruction
def instructions() -> Generator[List[int], None, None]:
    address: int = 0

    def gen() -> Generator[List[int], None, None]:
        nonlocal address
        while address + 1 < len(tape):
            opcode = memory[address]
            if not (opcode in instruction_lengths):
                raise ValueError(f"Bad opcode: {opcode} @ {address}\n{memory}")
            instruction_length = instruction_lengths[opcode]
            yield tape[address : (address + instruction_length)]
            address += instruction_length

    yield from gen()

# We have a generic function that takes the current instruction as a list of integers,
# looks up the function for the opcode indicated by the first integer, and passes
# the rest of the instruction as arguments to that function.
# It also returns the value of the function, unless it's an Exception, in which case
# it prints the error message of that Exception, and stops the program
def op(instruction: List[int]) -> None:
    opcode = instruction[0]
    instruction_parameters = instruction[1:]
    function = opcode_functions[opcode]
    try:
        result = function(*instruction_parameters)
    except StopIteration as err:
        print(*err.args)
        raise SystemExit()
    else:
        return result

# Lastly, we loop through all the instructions, and pass them to op
for instruction in instructions():
    op(instruction)
