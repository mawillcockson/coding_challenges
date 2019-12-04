from urllib.request import urlopen
from pathlib import Path

def file_cache(url: str) -> str:
    path = Path("day1_input.txt")
    if not path.is_file():
        req = urlopen(url)
        with open(str(path), "wb") as f:
            while (data := req.readline()) != b'':
                f.write(data)
    
    return path.read_text()

module_weights = map(int, file_cache("https://adventofcode.com/2019/day/1/input").split())
module_fuel = sum(
    map(
        lambda x: x // 3 - 2,
        module_weights
    )
)
print(module_fuel)