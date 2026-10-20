#include <stdio.h>

/*
The hostel in which you stop for the night changes its prices according to the
age of the customer and the weight of their luggage. The rules are not very
clear, so you decide to write a small program that will easily allow you and
your travel companions to know the price of one night.

One room costs nothing if you are exactly 60 (the age of the innkeeper), or 5
dollars if you are less than 10 years old. For everyone else, the cost is 30
dollars plus an additional 10 dollars if you bring more than 20 pounds of
luggage. Your program should read the customer's age first, then the weight of
their luggage, then output the price they have to pay.

Example:
Input:
22
25
Output: 40

Warning: You will be graded on your output, so do not include any print
statements that prompt a user for input.
*/

int main(void) {
    int age = 0;
    int luggageWeight = 0;
    int ret = 0;
    while (ret < 2) {
        ret = scanf("%d %d", &age, &luggageWeight);
        if (ret == EOF) return 1;
    }

    if (age == 60) {
        puts("0");
        return 0;
    }

    if (age < 10) {
        puts("5");
        return 0;
    }

    int cost = 30;
    if (luggageWeight > 20) cost += 10;

    printf("%d\n", cost);

    return 0;
}
