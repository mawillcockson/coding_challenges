import 'dart:math' as math;

// I feel like this should be possible without resorting to BigInt?

class ArmstrongNumbers {
  bool isArmstrongNumber(String number) {
    final int num_digits = number.length;
    // I should've known that the values would overflow the range of 64 bit
    // unsigned integers
    final int original = int.parse(number, radix: 10);
    List<int> nums = [];
    int value = original;
    while (value > 0) {
      final int remainder = value % 10;
      nums.add(remainder);
      value ~/= 10;
    }
    int sum = nums
        // just hope it doesn't overflow (unless there's a way to check for
        // possible truncation?)
        .map((x) => math.pow(x, nums.length).toInt())
        .fold(0, (a, b) => (a + b) % original);
    return sum == 0;
  }
}
