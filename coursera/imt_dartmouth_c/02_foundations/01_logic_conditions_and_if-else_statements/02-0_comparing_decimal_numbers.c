#include <stdio.h>

int main(void) {
    float a1 = 12345678.6;
    float b1 = 12345679.0;
    printf("float32 %22.12f == %22.12f -> %s\n", a1, b1, a1 == b1 ? "true" : "false");

    double a2 = 12345678.6;
    double b2 = 12345679.0;
    printf("float64 %22.12f == %22.12f -> %s\n", a2, b2, a2 == b2 ? "true" : "false");

    double a3 = 123456789.12345677;
    double b3 = 123456789.12345678;
    printf("float64 %22.12f == %22.12f -> %s\n", a3, b3, a3 == b3 ? "true" : "false");

    return 0;
}
