import math
import sys

assert sys.version_info >= (3, 9), "Requires Python 3.9 or higher"

print(f"{1.0:.52f}")
print(f"{math.nextafter(1.0, math.inf):.52f}")
