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

const int tinuviel =   1; // 2**0
const int calaelen =   2; // 2**1
const int falarion =   4; // 2**2
const int dorthonion = 8; // 2**3

int main(void) {
    int height = 0;
    int leaflets = 0;
    int ret = 0;
    while (ret < 2) {
        ret = scanf("%d %d", &height, &leaflets);
        if (ret == EOF) return 1;
    }
    
    int treeType = tinuviel + calaelen + falarion + dorthonion;

    if (height > 5) treeType ^= tinuviel;
    if (height > 8) treeType ^= falarion;
    if (height < 10) treeType ^= calaelen;
    if (height < 12) treeType ^= dorthonion;
    if (leaflets < 8) treeType ^= tinuviel;
    if (leaflets < 10) treeType ^= calaelen;
    if (leaflets > 5) treeType ^= falarion;
    if (leaflets > 7) treeType ^= dorthonion;

    switch (treeType) {
        case tinuviel: puts("Tinuviel"); return 0;
        case calaelen: puts("Calaelen"); return 0;
        case falarion: puts("Falarion"); return 0;
        case dorthonion: puts("Dorthonion"); return 0;
        default: puts("Unknown"); return 0;
    }
}
