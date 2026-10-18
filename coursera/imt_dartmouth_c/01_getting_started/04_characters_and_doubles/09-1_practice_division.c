#include <stdio.h>

/*
You just started learning a new language and decide to buy a few books to
practice. Thankfully you quickly find a book seller who offers every book for
the same low fixed price. You have a certain amount of money and you would like
to know how many books of the same price you can purchase.

Please write a C-program that starts by reading the amount of money you have
(which may be a double), then reads the price per book (which again may be a
double). The program should then display an integer, namely the largest number
of books that you can purchase with the given amount of money.

Examples:
Input:
48.0
3.50
Output: 13

Input:
27.0
5.0
Output: 5

Warning: You will be graded on your output, so do not include any print
statements that prompt a user for input.
*/

int main(void) {
    double money = 0;
    int readCount = 0;
    while (readCount < 1) {
        readCount = scanf("%lf", &money);
        if (readCount == EOF) return 1;
    }
    if (money <= 0) {
        printf("0");
        return 0;
    }

    double dollarsPerBook = 1;
    do {
        readCount = scanf("%lf", &dollarsPerBook);
        if (readCount == EOF) return 1;
    } while (readCount < 1);

    printf("%d", ((int) (money / dollarsPerBook)));

    return 0;
}
