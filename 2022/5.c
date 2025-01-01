#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 64
#define STACK_COUNT 9

struct crate {
  char c;
  struct crate *next;
};

void print_crates(struct crate **);
void move(struct crate **, int, int, int);

int main(void) {
  FILE *fp = fopen("5.txt", "r");
  char buf[BUF_SIZE];
  int moving = 0;
  struct crate **crates = malloc(sizeof(struct crate) * STACK_COUNT);

  while (fgets(buf, sizeof(buf), fp)) {
    if (buf[0] == '\n') {
      moving = 1;
    } else if (!moving) {
      for (int i = 0; i < STACK_COUNT; ++i) {
        char c = buf[1 + 4 * i];
        char d = buf[-1 + 4 * i];
        if (('A' <= c && c <= 'Z') && d != '\n') {
          struct crate *new = malloc(sizeof(struct crate));
          new->c = c;
          new->next = 0;
          if (!crates[i]) {
            crates[i] = new;
          } else {
            struct crate *tmp = crates[i];
            for (; tmp->next; tmp = tmp->next)
              ;
            tmp->next = new;
          }
        }
      }
    } else {
      int m, f, t;
      sscanf(buf, "move %d from %d to %d", &m, &f, &t);
      move(crates, m, f - 1, t - 1);
    }
  }

  for (int i = 0; i < STACK_COUNT; ++i) {
    printf("%c", crates[i]->c);
  }
  printf("\n");

  return 0;
}

void move(struct crate **crates, int m, int f, int t) {
  struct crate *start = crates[f];
  struct crate *end = start;
  for(int i=0;i<m-1;++i){
    end=end->next;
  }
  crates[f]=end->next;
  end->next=crates[t];
  crates[t]=start;
}
