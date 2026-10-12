#include <stdio.h>

/*
When Rémi came to the US (to visit Petra to make this MOOC) he brought his
favorite cookie recipe! When trying to bake the cookies he realized that ovens
in the US show temperature in degrees Fahrenheit, but the cookie recipe called
for a temperature in degrees Celsius. We need your help!

Please write a C-program that reads a decimal number representing a temperature
in degrees Celsius and prints out the corresponding temperature in degrees
Fahrenheit with 1 decimal place. Here is the conversion formula:

Temperature (°F) = Temperature (°C) × 9.0 / 5.0 + 32.0

Examples
Input: 192
Output: 377.6

Input: 30.5
Output: 86.9

Warning: You will be graded on your output, so do not include any print
statements that prompt a user for input.
*/

int main(void) {
  double celsius = 0;
  int readCount = 0;
  do {
    readCount = scanf("%lf", &celsius);
    if (readCount == EOF)
      return 1;
  } while (readCount < 1);
  printf("%.1f", (celsius * ((double)9 / 5)) + 32);
  return 0;
}
