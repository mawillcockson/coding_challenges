#include <stdio.h>

/*
You are helping a teacher average grades. You get bored computing averages by
hand, so you decide to write a computer program to do the work for you.

Your program must first read an integer indicating the number of grades to be
averaged. Next, your program will read the grades one by one, all of which are
integers as well. Finally, your program will calculate and print the average of
the grades to two decimal places.

Example:
Input:
4
10
8
16
9
Output: 10.75

Warning: You will be graded on your output, so do not include any print statements that prompt a user for input.
*/

int main(void) {
    int numGrades = 1;
    int total = 0;
    int readCount = 0;
    while (readCount < 1) {
        readCount = scanf("%d", &numGrades);
        if (readCount == EOF) return 1;
    }
    for (int i = 1; i <= numGrades; ++i) {
        int grade = 0;
        do {
            readCount = scanf("%d", &grade);
            if (readCount == EOF) return 1;
        } while (readCount < 1);
        total += grade;
    }
    printf("%.2f", ((double)total) / numGrades);
    return 0;
}
