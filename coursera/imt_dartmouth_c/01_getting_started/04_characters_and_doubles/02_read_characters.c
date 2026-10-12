#include <stdio.h>

const int width = 9;
int main(void) {
    char fill_char = '\v';
    int result = 0;
    do {
        result = scanf("%c", &fill_char);
        if (result == EOF) return 1;
    } while (result < 1);

    for (int plusses = 4; plusses >= 0; --plusses) {
        for (int i = plusses; i > 0; --i) 
            printf("+");

        for (int fill = width - (2 * plusses); fill > 0; --fill) 
            printf("%c", fill_char);

        for (int i = plusses; i > 0; --i) 
            printf("+");

        printf("\n");
    }

    return 0;
}
