from typing import Tuple, Union
from pathlib import Path
from operator import add, mul

memory = list(
    map(
        int,
        Path("day2_input.txt").read_text().split(","),
    )
)
pointer: int = 0

# Adjust memory
memory[1] = 12
memory[2] = 2

def op(opcode: int, val1: int, val2: int, output: int) -> None:
    if not isinstance(opcode, int) or opcode not in [1,2,99]:
        raise NotImplementedError(
            f"instruction:\n"
            f"{opcode!r}\n"
            f"memory:\n"
            f"{','.join(map(str, memory))}"
    )
    if opcode == 99:
        print(f"STOP: {memory[0]}")
        raise StopIteration

    func, s = (add, '+') if opcode == 1 else (mul, '*')
    print(f"{s} {val1: >4d} {val2: >4d} = {(res := func(val1, val2)): >4d} -> {output: >4d}")
    memory[output] = res

#def fetch() -> Union[Tuple[int, int, int], int]:
#    # Why do I have to specify global?
#    global pointer
#    vals = list()
#    for i in range(pointer, pointer + 4):
#        vals.append(memory.get(i, None))
#
#    if vals[i] not in [1, 2, 99]:
#        raise NotImplementedError(
#            f"memory:\n{','.join(memory.values())}\n"
#            f"pointer: {pointer}\n"
#            f"current instruction: {' '.join(vals)}"
#        )
#    if vals[0] != "99" and None in vals:
#        raise MemoryError(
#            f"memory:\n{','.join(memory.values())}\n"
#            f"pointer: {pointer}\n"
#            f"current instruction: {' '.join(vals)}"
#        )
#    print(f"current instruction:",
#          f"{' '.join(vals)}",
#          sep="\n",
#    )
#    return tuple(*vals)

while (data := memory[pointer:pointer + 4]):
    op(*data)
    pointer += 4