// I feel like this should be possible without resorting to BigInt?

// Using BigInts in this version

class ArmstrongNumbers {
  static bool isArmstrongNumber(String number) {
    final int num_digits = number.length;
    final BigInt original = BigInt.parse(number, radix: 10);
    List<BigInt> nums = [];
    BigInt value = original;
    while (value > BigInt.from(0)) {
      final remainder = value.remainder(BigInt.from(10));
      nums.add(remainder);
      value ~/= BigInt.from(10);
    }
    BigInt sum = nums
        // just hope it doesn't overflow (unless there's a way to check for
        // possible truncation?)
        .map((BigInt x) => x.pow(nums.length))
        .fold(BigInt.from(0), (BigInt a, BigInt b) => a + b);
    return sum.compareTo(original) == 0;
  }
}
