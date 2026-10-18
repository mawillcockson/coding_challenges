#include <assert.h>
#include <limits.h>
#include <stdio.h>

union float_int {
    float float_;
    unsigned int int_;
};

int main(void) {
    // can an unsigned int hold a value representing the number of bits in a float?
    assert(UINT_MAX >= (sizeof(float) * CHAR_BIT));
    // is an unsigned int at least as big as a float?
    assert(sizeof(unsigned int) >= sizeof(float));

    union float_int number;
    union float_int mask;
    //number.float_ = 0;
    //number.float_ = 1;
    //number.int_ = 0xFF << 23; // infinity
    //number.int_ = (0xFF << 23) + 1; // NaN
    number.int_ = 0x1FF << 22; // -infinity
    mask.int_ = 0;

    printf("a float of value %f looks like: ", number.float_);
    
    unsigned int checker = (sizeof(float) * CHAR_BIT);
    while (checker > 0) {
        --checker;
        //fprintf(stderr, "checking bit %d\n", checker);
        mask.int_ = 1;
        mask.int_ <<= checker;
        if ((number.int_ & mask.int_) == 0) {
            putc('0', stdout);
        } else {
            putc('1', stdout);
        }
    }
    putc('\n', stdout);

    return 0;
}
