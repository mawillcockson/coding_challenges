#include <stdio.h>

const int width = 9;
int main(void) {
  char fill_char = '\v';
  int result = 0;
  do {
    result = scanf("%c", &fill_char);
    if (result == EOF)
      return 1;
  } while (result < 1);

  printf("++++%c++++", fill_char);
  printf("+++%c%c%c+++", fill_char, fill_char, fill_char);
  printf("++%c%c%c%c%c++", fill_char, fill_char, fill_char, fill_char,
         fill_char);
  printf("+%c%c%c%c%c%c%c+", fill_char, fill_char, fill_char, fill_char,
         fill_char, fill_char, fill_char);
  printf("%c%c%c%c%c%c%c%c%c", fill_char, fill_char, fill_char, fill_char,
         fill_char, fill_char, fill_char, fill_char, fill_char);

  return 0;
}
