#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 256
#define GROUP_SIZE 3

char find_badge(char **);
int get_points(char);

int main(void) {
  FILE *fp = fopen("3.txt", "r");
  char buf[BUF_SIZE];
  char **group = malloc(sizeof(char *) * GROUP_SIZE);
  for (int i = 0; i < GROUP_SIZE; ++i) {
    group[i] = malloc(BUF_SIZE);
  }
  int points = 0;

  for (int i = 0; fgets(buf, sizeof(buf), fp); ++i) {
    if (i == GROUP_SIZE) {
      i = 0;
      points += get_points(find_badge(group));
    }
    strcpy(group[i], buf);
    group[i][strlen(group[i]) - 1] = '\0';
  }
  points += get_points(find_badge(group));
  printf("%d\n", points);
  fclose(fp);
  return 0;
}

char find_badge(char **group) {
  char match[BUF_SIZE];
  strcpy(match, group[0]);
  for (int i = 1; i < GROUP_SIZE; ++i) {
    char tmp[BUF_SIZE] = "";
    int match_len = strlen(match);
    for (int j = 0; j < match_len; ++j) {
      char c[2] = {match[j], '\0'};
      if (strchr(tmp, c[0])) {
      } else if (strchr(group[i], c[0])) {
        strcat(tmp, c);
      }
    }
    strcpy(match, tmp);
  }
  return match[0];
}

int get_points(char c) {
  int tmp = (c < 'a') ? c - 38 : c - 96;
  return tmp;
}
