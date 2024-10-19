//import 'dart:typed_data' as typed_data;
import 'dart:math' as math;

class ArmstrongNumbers {
  bool isArmstrongNumber(String number) {
    final MyBigInt original = MyBigInt.parse(number);
    final List<MyBigInt> nums = [];
    MyBigInt value = original;
    int numDigits = 0;
    while (value.bitLength > 0) {
      final (:quotient, :remainder) = value.divMod(MyBigInt.from(10));
      print('q, r -> $quotient, $remainder');
      nums.add(remainder);
      value = quotient;
      ++numDigits;
    }
    // just hope it doesn't overflow (unless there's a way to check for
    // possible truncation?)
    print('numDigits -> $numDigits');
    print('nums -> $nums');
    nums.forEach((MyBigInt x) => x.pow(numDigits));
    print('nums ** numDigits -> $nums');
    MyBigInt sum = nums.fold(MyBigInt.from(0), (MyBigInt a, MyBigInt b) {
      a.add(b);
      return a;
    });
    print('sum -> $sum');
    try {
      sum.subtract(original);
    } on NegativityException {
      return false;
    }
    return sum.isZero;
  }
}

void main() {
  final bool Function(String) isAN = ArmstrongNumbers().isArmstrongNumber;
  bool seven = isAN('7');
  print(seven);
  assert(seven);
  print('---');
  bool eleven = isAN('11');
  print(eleven);
  print(MyBigInt.parse('11'));
}

extension IndexableList<T> on List<T> {
  T? getAt(int index) {
    try {
      return this[index];
    } on RangeError {
      return null;
    }
  }

  T getAtDefault(int index, T otherwise) => this.getAt(index) ?? otherwise;

  void fillRangeTo(int index, T fill) {
    if (index <= this.length - 1) {
      return;
    }
    this.addAll(Iterable.generate(index - (this.length - 1), (x) => fill));
  }

  void setAt(int index, T value, T fill) {
    if (index > this.length - 1) {
      this.fillRangeTo(index, fill);
    }
    this[index] = value;
  }
}

typedef Bit = int;

typedef Bits = List<Bit>;

final int asciiDigitOffset = "0".runes.cast<int>().single;

class NegativityException implements Exception {}

class MyBigInt {
  final Bits _bits = [];

  MyBigInt(Bits bits) {
    _bits.addAll(bits);
  }

  static parse(String number) {
    MyBigInt bigInt = MyBigInt.from(0);

    int magnitude = 1;
    for (final int codepoint in number.runes.cast<int>()) {
      int value = codepoint - asciiDigitOffset;
      print('value -> $value');
      bigInt.addInt(value * math.pow(10, magnitude).toInt());
      ++magnitude;
    }
    return bigInt;
  }

  bool get isPositive => this._bits.lastIndexOf(1) >= 0;
  bool get isZero => this._bits.lastIndexOf(1) < 0;
  int get bitLength => this._bits.lastIndexOf(1) + 1;

/*
3: 0011
7: 0111

0110
c: 1

0110
c: 1

0010
c: 1

1010
c: 0
*/

  void addInt(int other) {
    if (other is! int || other < 0) {
      throw Exception('use subtractInt() for negative numbers');
    }
    this.add(MyBigInt.from(other));
  }

  void add(MyBigInt other) {
    Bit carry = 0;
    const Bit fill = 0;
    for (int i = 0; i <= other.bitLength - 1; ++i) {
      final Bit bit = other._bits[i];
      final Bit currentValue = this._bits.getAtDefault(i, 0);
      switch ((currentValue, bit, carry)) {
        case (0, 0, 0):
          this._bits.setAt(i, 0, fill);
        case (1, 0, 0):
        case (0, 1, 0):
          this._bits.setAt(i, 1, fill);
        case (0, 0, 1):
          this._bits.setAt(i, 1, fill);
          carry = 0;
        case (1, 1, 0):
          this._bits.setAt(i, 0, fill);
          carry = 1;
        case (0, 1, 1):
        case (1, 0, 1):
          this._bits.setAt(i, 0, fill);
          carry = 1;
        case (1, 1, 1):
          this._bits.setAt(i, 1, fill);
          carry = 1;
        default:
          throw Exception('Should not have reached this!');
      }
    }
    if (carry > 0) {
      this._bits.add(carry);
    }
  }

  MyBigInt operator +(MyBigInt other) {
    MyBigInt copy = this.copy();
    copy.add(other);
    return copy;
  }

  MyBigInt operator -(MyBigInt other) {
    MyBigInt copy = this.copy();
    copy.subtract(other);
    return copy;
  }

  MyBigInt copy() => MyBigInt(this._bits.toList());

/*
8: 1000
7: 0111

b: 0
m: 0
s: 1
d= 1
b= 1

b: 1
m: 0
s: 1
d= 0
b= 1

b: 1
m: 0
s: 1
d= 0
b= 1

b: 1
m: 1
s: 0
d= 0
b= 0

0001
*/

  void subtractInt(int other) => this.subtract(MyBigInt.from(other));

  void subtract(MyBigInt other) {
    if (other.bitLength > this._bits.length) {
      throw NegativityException();
    }
    print('other.bitLength -> ${other.bitLength}');
    const Bit fill = 0;
    int borrow = 0;
    for (int i = 0; i <= other.bitLength - 1; ++i) {
      final Bit subtrahend = other._bits[i];
      final Bit minuend = this._bits.getAtDefault(i, 0);
      print('(m, s, b) -> ($minuend, $subtrahend, $borrow)');
      switch ((minuend, subtrahend, borrow)) {
        case (0, 0, 0):
        case (1, 0, 1):
        case (1, 1, 0):
          print('case group 1');
          this._bits.setAt(i, 0, fill);
          borrow = 0;
        case (1, 0, 0):
          print('case group 2');
          this._bits.setAt(i, 1, fill);
          borrow = 0;
        case (0, 0, 1):
        case (0, 1, 0):
        case (1, 1, 1):
          print('case group 3');
          this._bits.setAt(i, 1, fill);
          borrow = 1;
        case (0, 1, 1):
          print('case group 4');
          this._bits.setAt(i, 0, fill);
          borrow = 1;

        default:
          print('default case');
          throw Exception('Should not have reached this!');
      }
    }
    if (borrow != 0) {
      throw NegativityException();
    }
  }

  ({MyBigInt quotient, MyBigInt remainder}) divMod(MyBigInt divisor) {
    if (!divisor.isPositive) {
      throw Exception('divisor should be positive');
    }
    MyBigInt count = MyBigInt.from(0);
    MyBigInt copy = this.copy();
    while (true) {
      try {
        copy = copy - divisor;
        count.addInt(1);
      } on NegativityException {
        break;
      }
    }
    return (quotient: count, remainder: copy);
  }

  MyBigInt operator %(MyBigInt modulus) => this.divMod(modulus).remainder;
  MyBigInt operator ~/(MyBigInt divisor) => this.divMod(divisor).quotient;

  int toInt() {
    int sum = 0;
    final int end = this._bits.lastIndexOf(1);
    for (int i = 0; i <= end; ++i) {
      sum += math.pow(2, i).toInt() * this._bits[i];
    }
    return sum;
  }

  MyBigInt.from(int number) {
    if (number is! int || number < 0) {
      throw Exception('can only cast positive integers');
    }
    if (number == 0) {
      this._bits.add(0);
      return;
    }

    int value = number;
    while (value > 0) {
      final int remainder = value % 2;
      this._bits.add(remainder as Bit);
      value ~/= 2;
    }
  }

  void pow(int power) {
    if (power is! int || power < 0) {
      throw Exception('can only raise to the power of positive integers');
    }

    MyBigInt original = this.copy();
    for (int i = 1; i < power; ++i) {
      this.add(original);
    }
  }

  String toString() {
    return this._bits.reversed.join();
  }
}

/*
class BitBuilder implements typed_data.BytesBuilder {
  final typed_data.ByteData _b;
  int _bitsLength = 0;

  int get bitsLength => _bitsLength;

  void add(List<int> bytes) => _b.add(bytes);
  void addByte(int byte) => _b.addByte(byte);
  void clear() => _b.clear();
  bool get isEmpty => _b.isEmpty;
  bool get isNotEmpty => _b.isNotEmpty;
  typed_data.Uint8List takeBytes() => _b.takeBytes();
  typed_data.Uint8List toBytes() => _b.toBytes();
  int get length => _b.length;

  factory BitBuilder() {
    return BitBuilder._internal(typed_data.BytesBuilder());
  }

  BitBuilder._internal(this._b);

  List<Bits> toBits() {
      return this._b.toBytes()
  }
}
*/
