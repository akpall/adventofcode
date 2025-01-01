#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
  FILE *fp = fopen("2.txt", "r");
  char buf[4];
  int points = 0;

  while (fgets(buf, 4, fp)) {
    switch (buf[0]) {
    case 'A':
      switch (buf[2]) {
      case 'X':
        points += 3;
        break;
      case 'Y':
        points += 4;
        break;
      case 'Z':
        points += 8;
        break;
      }
      break;
    case 'B':
      switch (buf[2]) {
      case 'X':
        points += 1;
        break;
      case 'Y':
        points += 5;
        break;
      case 'Z':
        points += 9;
        break;
      }
      break;
    case 'C':
      switch (buf[2]) {
      case 'X':
        points += 2;
        break;
      case 'Y':
        points += 6;
        break;
      case 'Z':
        points += 7;
        break;
      }
      break;
    }
  }
  printf("%d\n", points);

  fclose(fp);
  return 0;
}
