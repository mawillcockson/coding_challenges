from urllib.request import urlopen
from pathlib import Path

inputs = Path("day1_input.txt").read_text()
module_weights = map(int, inputs.split())

def sum_with_fuel(weight: int) -> int:
    weights = [weight]
    while weights[-1] // 3 - 2 > 0:
        weights.append( weights[-1] // 3 - 2 )
    
    return sum(weights[1:])

module_fuel = sum(
    map(
        sum_with_fuel,
        module_weights
    )
)
print(module_fuel)