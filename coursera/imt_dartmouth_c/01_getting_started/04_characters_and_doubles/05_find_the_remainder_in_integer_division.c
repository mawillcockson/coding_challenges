#include <stdio.h>

/*
You have a number of loose matches that you would like to
put back into boxes. Write a program that calculates and
displays how many full boxes you will have and how many
leftover matches you will have after filling all the boxes
you can. Your program should take as input the number of
matches to be boxed up followed by the size of a each box.
Next it should print out the number of full boxes followed
by the number of remaining matches.

Examples:
Input:
666
13
Output: 
51
3
*/

int main(void) {
    int matches_per_box = 1;
    int total_matches = 0;
    int readCount = 0;
    do {
        readCount = scanf("%d", &total_matches);
        if (readCount == EOF) return 1;
    } while (readCount < 1);
    do {
        readCount = scanf("%d", &matches_per_box);
        if (readCount == EOF) return 1;
    } while (readCount < 1);

    printf("%d\n%d\n", total_matches / matches_per_box, total_matches % matches_per_box);
    return 0;
}
