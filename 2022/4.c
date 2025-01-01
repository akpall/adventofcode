#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 32

int main(void) {
  FILE *fp = fopen("4.txt", "r");
  char buf[BUF_SIZE];
  int points = 0;

  while (fgets(buf, sizeof(buf), fp)) {
    char *token = strtok(buf, ",");
    char tokens[2][BUF_SIZE / 2];
    int ID[4];
    int buf_len = strlen(buf);

    for (int i = 0; i < 2; ++i) {
      strcpy(tokens[i], token);
      token = strtok(0, ",");
    }

    for (int i = 0; i < 2; ++i) {
      token = strtok(tokens[i], "-");
      for (int j = i * 2; j < 2 + i * 2; ++j) {
        ID[j] = strtol(token, 0, 10);
        token = strtok(0, "-");
      }
    }

    for (int i = ID[0]; i <= ID[1]; ++i) {
      for (int j = ID[2]; j <= ID[3]; ++j) {
        if (i == j) {
          points++;
          goto EXIT;
        }
      }
    }
  EXIT:;
  }
  printf("%d\n", points);

  fclose(fp);
  return 0;
}
