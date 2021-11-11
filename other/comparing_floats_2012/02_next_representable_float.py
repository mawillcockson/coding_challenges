import math
import sys

MANTISSA_DIGITS = sys.float_info.mant_dig
SMALLEST_INTERVAL = sys.float_info.epsilon


print(f"{1.0:.{MANTISSA_DIGITS}f}")
print(f"{1.0 + SMALLEST_INTERVAL:.{MANTISSA_DIGITS}f}")
