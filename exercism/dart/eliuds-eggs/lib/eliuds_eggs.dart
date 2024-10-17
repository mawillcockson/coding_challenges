class EggCounter {
  int count(int reading) {
    int bit_check = 1 << 0;
    int bits = 0;
    // scan through the bits of the reading, comparing it with a single check
    // bit in ever-ascending positions of significance, keeping track of how
    // many times the result is positive, and stopping when the check value has
    // shifted outside the range of the reading's possible bit positions
    while (bit_check <= reading) {
      if ((reading & bit_check) > 0) {
        bits += 1;
      }
      bit_check <<= 1;
    }
    return bits;
  }
}
