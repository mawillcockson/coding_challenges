import strformat

const f: float32 = 0.1
var sum: float32
sum = 0

for i in 1..10:
  sum += f
let product: float32 = f * 10
echo &"sum  = {sum:1.15f}\nmul1 = {product:1.15f}\nmul2 = {(block:f * 10):1.15f}"
