#include <stdio.h>

/*
As you cross a forest you can't help but admire the nature around you including
the many species of trees. Despite your interest, you are a very unskilled
botanist and have a lot of trouble identifying different trees. A friend gives
you some guidance and you decide to write a program that will give you the name
of the tree based on its characteristics.

There are 4 types of trees:
- the "Tinuviel" is 5 meters high or less and its leaves are composed of 8 or more leaflets
- the "Calaelen" is 10 meters high or more and its leaves are composed of 10 or more leaflets
- the "Falarion" is 8 meters high or less and its leaves are composed of 5 or fewer leaflets
- the "Dorthonion" is 12 meters tall or more and its leaves are composed of 7 or fewer leaflets

Your program should read the height and the number of leaflets of a given tree
(both integers), and should be able to determine and display the name of the
corresponding tree.  If the height and number of leaflets does not match any of
the tree type descriptions, your program should display "Uncertain".

Examples:
Input:
12
12
Output: Calaelen

Input:
4
9
Output: Tinuviel

Input:
4
6
Output: Uncertain

Warning: You will be graded on your output, so do not include any print
statements that prompt a user for input.
*/

#define TINUVIEL   1 // 2**0
#define CALAELEN   2 // 2**1
#define FALARION   4 // 2**2
#define DORTHONION 8 // 2**3

int main(void) {
    int height = 0;
    int leaflets = 0;
    int ret = 0;
    while (ret < 2) {
        ret = scanf("%d %d", &height, &leaflets);
        if (ret == EOF) return 1;
    }
    
    int treeType = TINUVIEL + CALAELEN + FALARION + DORTHONION;

    if (height > 5) treeType &= ~TINUVIEL;
    if (height > 8) treeType &= ~FALARION;
    if (height < 10) treeType &= ~CALAELEN;
    if (height < 12) treeType &= ~DORTHONION;
    if (leaflets < 8) treeType &= ~TINUVIEL;
    if (leaflets < 10) treeType &= ~CALAELEN;
    if (leaflets > 5) treeType &= ~FALARION;
    if (leaflets > 7) treeType &= ~DORTHONION;

    switch (treeType) {
        case TINUVIEL: puts("Tinuviel"); return 0;
        case CALAELEN: puts("Calaelen"); return 0;
        case FALARION: puts("Falarion"); return 0;
        case DORTHONION: puts("Dorthonion"); return 0;
        default: puts("Uncertain"); return 0;
    }
}
