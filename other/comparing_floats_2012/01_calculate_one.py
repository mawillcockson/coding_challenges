"""
from this article:
https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
"""
f: float = 0.1
sum: float = 0

for _ in range(10):
    sum += f
product: float = f * 10
print(f"""sum  = {sum:.15f}
mul1 = {product:.15f}
mul2 = {f * 10:.15f}""")
