import sys
from pathlib import Path
import inspect
from itertools import combinations
from typing import Generator, Tuple, Union, List

# We have an input tape that lists out the memory in a file
# We read it in, and turn it into a list of integers
memory: List[int]
memory = list(
    map(
        int,
        Path("day2_input.txt").read_text().split(","),
    )
)
# This tape is now a representation of our memory

# For part 2, we vary the values in these memory address locations
#memory[1] = 12
#memory[2] = 2

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
    # We'll be running this function a lot, so we'll not print out every operation
    #print(f"+ {val1: >7} {val2: >7} = {result: >7} -> {out: >7}")

def op2(arg1: int, arg2: int, out: int) -> None:
    '''opcode 1 -> multiplies together the values found in memory, using the first two
       arguments as addresses, and stores the result in memory at the adress pointed to
       by the third argument
    '''
    val1 = memory[arg1]
    val2 = memory[arg2]
    result = val1 * val2
    memory[out] = result
    #print(f"* {val1: >7} {val2: >7} = {result: >7} -> {out: >7}")

# The STOP opcode is simple, and an exception to stop the machine
# In the problem statement, the memory position 0 holds the answer,
# so we return that with the Exception
def op99() -> None:
    return memory[0]

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
        while address + 1 < len(memory):
            opcode = memory[address]
            if not (opcode in instruction_lengths):
                raise ValueError(f"Bad opcode: {opcode} @ {address}\n{memory}")
            instruction_length = instruction_lengths[opcode]
            yield memory[address : (address + instruction_length)]
            address += instruction_length

    yield from gen()

# We have a generic function that takes the current instruction as a list of integers,
# looks up the function for the opcode indicated by the first integer, and passes
# the rest of the instruction as arguments to that function.
# It also returns the value of the function
def op(instruction: List[int]) -> None:
    opcode = instruction[0]
    instruction_parameters = instruction[1:]
    function = opcode_functions[opcode]
    return function(*instruction_parameters)

# For part 2, we have to vary the value sin memory at position 1, called the noun,
# and address 2, called the verb, with values ranging from 1 to 99 inclusive, and find
# the correct combination that causes the program to halt with the address 0 value equal
# to the desired value.
# For every loop through, we have to make sure we're using the same initial memory.
# Instead of reading in the memory potentially thousands of times, we'll make a copy,
# and restore the memory from that copy for each new run of the program.
# We'll stop when, after a run, the value at address 0 is our desired result, and print the
# value 100 * noun + verb
desired_value = 19690720
# Initialize empty array for our copy of memory
memory_copy = []
# Fill it with the current, clean memory
memory_copy.extend(memory)
for noun, verb in combinations(range(1, 99 + 1), 2):
    # Print out the current noun and verb on each loop
    print(f"{noun} {verb}")
    # Erase current memory
    memory.clear()
    # Fill it with our clean copy
    memory.extend(memory_copy)
    # Place our current noun and verb values in memory
    memory[1] = noun
    memory[2] = verb
    for instruction in instructions():
        result = op(instruction)
        if result == desired_value:
            print(f"STOP: {100 * noun + verb}")
            raise SystemExit()
