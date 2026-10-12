#include <stdio.h>
#include <stdlib.h>

/*
Here is your final activity of this unit. Use it to apply everything you have learned! David is fighting Goliath (again...) and it turns out that Goliath is much bigger than David thought. Fortunately David is not short of resources and he plans to send robots to fight the giant. But before launching the assault, David must evaluate the performance of these robots to ensure success. This is where you come in. You are given some data on David's robots and need to compute and output a corresponding power score.

Here is how: You should write a program that takes several lines of input from a user (see the below example). The first line of the input indicates the number of robots to be deployed. Each subsequent line shows the height, the weight, the power of the engines and a resistance rating (1,2, or 3) of each of of these robots. Your program should use this information to calculate the total power of these robots. The power of all robots is the sum of the power of each robot, where the power of an individual robot is calculated via:

(enginePower + resistance) * (weight - height)

Example
Input:

2
50 60 2 1
43 62 5 2

Output:

163

Here the output in this example is the calculation

(2 + 1) * (60-50) + (5 + 2) * (62-43)

You must use a loop to read each of the lines!

Warning: Your program must allow David to evaluate any army he gives as an input, not just the one given as an example.
Note
*/

/* For scanf()'s return value, and exit(), I referenced
 * ISO/IEC JTC1 SC22 WG14 N1570 [1], since that's what musl's documentation [2]
 * suggests using, and musl is what zig's libc uses [3], and I'm using zig to
 * compile this
 *
 * [1]: https://www.open-std.org/JTC1/SC22/WG14/www/docs/n1570.pdf
 * [2]: https://musl.libc.org/doc/1.1.24/manual.html
 * [3]: https://ziglang.org/download/0.17.0/release-notes.html#musl-125
 *
 * Actually, it looks like on Windows:
 * $ zig cc -print-effective-triple
 * > x86_64-unknown-windows-gnu
 *
 * and `-target x86_64-windows-musl` reports:
 * > error: unable to provide libc for target "x86_64-windows.win10...win11_kr-musl"
 * > info: zig can provide libc for related target x86_64-windows-gnu
 *
 * So I should actually reference:
 * https://ziglang.org/download/0.17.0/release-notes.html#glibc-244
 * https://sourceware.org/glibc/manual/latest/html_node/Library-Summary.html
 * https://sourceware.org/glibc/manual/latest/html_node/Normal-Termination.html
 * https://sourceware.org/glibc/manual/latest/html_node/Formatted-Input-Functions.html
 *
 * The command I'm using to compile this file is:
 * zig cc \
 *     -o 000_reading_user_input_inside_a_loop.exe \
 *     000_reading_user_input_inside_a_loop.c \
 *     --std=c23 -Wall -Wpedantic -target x86_64-windows-gnu
 *
 * And for fun:
 * zig cc \
 *     -o 000_reading_user_input_inside_a_loop \
 *     000_reading_user_input_inside_a_loop.c \
 *     --std=c23 -Wall -Wpedantic -target x86_64-linux-musl \
 *     -Os -s -flto
 */

void check_result(int* result) {
    if (*result == EOF) {
        printf("EOF");
        exit(1);
    }
}

int main(void) {
    /**
    {
        int temp1, temp2;
        int result = scanf("%d%d", &temp1, &temp2);
        printf("temp1: %d\ntemp2: %d\nresult: %d", temp1, temp2, result);
    }
    **/

    // The first line of the input indicates the number of robots to be deployed.
    int numRobots = 0;
    int result = 0;
    do {
        result = scanf("%d", &numRobots);
        check_result(&result);
    } while (result < 1);

    // calculate the total power of these robots. The power of all robots is the sum of the power of each robot
    int sum = 0;
    for (int i = 1; i <= numRobots; ++i) {
        // Each subsequent line shows the height, the weight, the power of the engines and a resistance rating (1,2, or 3) of each of of these robots
        int height = 0;
        int weight = 0;
        int power = 0;
        int resistance = 0;
        do {
            result = scanf("%d%d%d%d", &height, &weight, &power, &resistance);
            check_result(&result);
        } while (result < 4 || (!(1 <= resistance && resistance <= 3)));

        // (enginePower + resistance) * (weight - height)
        int thisRobot = (power + resistance) * (weight - height);

        sum += thisRobot;
    }

    printf("%d", sum);
}
