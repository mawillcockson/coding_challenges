#include <assert.h>
#include <limits.h>
#include <stdio.h>

union float_int {
    float float_;
    unsigned int int_;
};

const unsigned int bitsInFloat = sizeof(float) * CHAR_BIT;
const int significandPrecisionBits = 23; // could derive this, to support platforms where a float isn't 32 bits

void printFloatBits(float num) {
    union float_int number;
    number.float_ = num;

    char bits[bitsInFloat];
    for (unsigned int bit = bitsInFloat; bit > 0; --bit) {
        //fprintf(stderr, "checking bit %d\n", bit);
        if (((number.int_ >> (bit - 1)) & 1) == 0) {
            bits[bit - 1] = '0';
        } else {
            bits[bit - 1] = '1';
        }
    }

    printf("a float with a value of %12.1f looks like: ", number.float_);
    for (unsigned int bit = bitsInFloat; bit > 0; --bit) {
        putc(bits[bit - 1], stdout);
    }
    putc('\n', stdout);
}

int main(void) {
    /* can an unsigned int hold a value representing the number of bits in a float?
     * NOTE::BUG can this be checked at compile-time, or in a way where the
     * math doesn't overflow the types the compiler might use for the
     * calculation?
     */
    assert(UINT_MAX >= (sizeof(float) * CHAR_BIT));

    printFloatBits(0);
    printFloatBits(1);
    printFloatBits(-1);
    printFloatBits(0.1);
    printFloatBits(-0.1);
    printFloatBits(1 << 2);
    printFloatBits(1 << 3);
    printFloatBits(1 << 4);
    printFloatBits(1 << 5);
    printFloatBits(1 << 10);
    printFloatBits(1 << 20);
    printFloatBits(1 << 30);
    union float_int number;
    number.int_ = 0xFF << significandPrecisionBits; // infinity
    printFloatBits(number.float_);
    number.int_ = (0xFF << significandPrecisionBits) + 1; // NaN
    printFloatBits(number.float_);
    number.int_ = (0xFF << significandPrecisionBits) + (1 << 1); // NaN
    printFloatBits(number.float_);
    number.int_ = (0xFF << significandPrecisionBits) + (1 << 2); // NaN
    printFloatBits(number.float_);
    number.int_ = (0xFF << significandPrecisionBits) + (1 << 5) + (1 << 3); // NaN
    printFloatBits(number.float_);
    number.int_ = UINT_MAX; // NaN
    printFloatBits(number.float_);
    number.int_ = (0xFF << significandPrecisionBits) | ((unsigned int)1 << (bitsInFloat - 1)); // -infinity
    printFloatBits(number.float_);

    return 0;
}
