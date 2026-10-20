#include <stdio.h>

/*
You arrive in front of a bridge that you must cross to reach a village before
dark. Crossing the bridge is not free - the bridgekeeper asks you to roll two
dice to determine the cost. You decide to write a program to verify that he is
charging the right price.

Your program should read two integers, between 1 and 6, representing the values
of each die. If the sum is greater than or equal to 10, then you must pay a
special fee (36 coins). Otherwise, you pay twice the sum of the values of the
two dice. Your program must then display the text "Special tax" or "Regular
tax" followed by the amount you have to pay on the next line.

Examples:
Input:
5
6
Output:
Special tax
36
 
Input:
4
3
Output:
Regular tax
14

Warning: You will be graded on your output, so do not include any print
statements that prompt a user for input.
*/

int main(void) {
    int dice1 = 0;
    int dice2 = 0;
    int ret = 0;
    while (ret < 2 || dice1 < 1 || dice2 < 1 || dice1 > 6 || dice2 > 6) {
        ret = scanf("%d %d", &dice1, &dice2);
        if (ret == EOF) return 1;
    }

    int sum = dice1 + dice2;
    if (sum >= 10) {
        puts("Special tax\n36");
        return 0;
    }
    printf("Regular tax\n%d\n", sum * 2);

    return 0;
}
