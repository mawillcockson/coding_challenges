"""
from this article:
https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
"""
import sys

MANTISSA_DIGITS = sys.float_info.mant_dig
f: float = 0.1
sum: float = 0

for _ in range(10):
    sum += f
product: float = f * 10
print(f"""sum  = {sum:.{MANTISSA_DIGITS}f}
mul1 = {product:.{MANTISSA_DIGITS}f}
mul2 = {f * 10:.{MANTISSA_DIGITS}f}""")
