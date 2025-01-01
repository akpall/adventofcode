#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
  FILE *fp = fopen("1.txt", "r");
  char buf[256];
  int most[3] = {};
  int tmp = 0;

  while (fgets(buf, sizeof(buf), fp)) {
    if (buf[0] == '\n') {
      for (int i = 0; i < 3; ++i) {
        if (tmp > most[i]) {
          most[i] += tmp;
          tmp = most[i] - tmp;
          most[i] -= tmp;
        }
      }
      tmp = 0;
    } else {
      tmp += strtol(buf, 0, 10);
    }
  }
  for (int i = 0; i < 3; ++i) {
    if (tmp > most[i]) {
      most[i] += tmp;
      tmp = most[i] - tmp;
      most[i] -= tmp;
    }
  }
  tmp = 0;
  for (int i = 0; i < 3; ++i) {
    tmp += most[i];
  }
  printf("%d\n", tmp);
  fclose(fp);
  return 0;
}
