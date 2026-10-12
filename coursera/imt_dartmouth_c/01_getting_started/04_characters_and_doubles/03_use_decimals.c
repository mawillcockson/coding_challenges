#include <stdio.h>

/*
Petra, Rémi and their families went hiking in the mountains together and
realized that distances are measured in different units in France and the
United States. To help them convert between units, please write a program that
reads a decimal number representing a distance in kilometers and that prints
out the corresponding distance in miles with 6 decimal places.

You may use the fact that one kilometer equals 0.621371 miles.

Example Input: 4.8
Output: 2.982581
*/

// ISO/IEC JTC1 SC22 WG14 N1570 sec. 7.21.6.7 states
// `l' length modifier: or has no effect on a following a, A, e, E, f, F, g, or
// G conversion specifier.
// Yet this course uses it in all the examples.
//
// Answer: I was reading about the format specification for printf(); scanf()
// does care about %lf

const double miles_per_kilometer = 0.621371;

int main(void) {
  double kilometers = 0;
  int result = 0;
  do {
    result = scanf("%lf", &kilometers);
    if (result == EOF)
      return 1;
  } while (result < 1);
  printf("%.6f", kilometers * miles_per_kilometer);
  return 0;
}
