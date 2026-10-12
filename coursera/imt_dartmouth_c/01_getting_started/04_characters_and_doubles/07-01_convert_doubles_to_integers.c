#include <stdio.h>
int main(void) {
    double dOne, dTwo;
    int iOne, iTwo;
    printf("Please enter two decimals: ");
    scanf("%lf %lf", &dOne, &dTwo);
    printf("I read dOne = %lf, dTwo = %lf.\n", dOne, dTwo);
    iOne = dOne;
    iTwo = dTwo;
    printf("As integers: %d %d", iOne, iTwo);
    return 0;
}
