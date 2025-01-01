#include <stdio.h>

#define PACKET_SIZE 14

int check_packet();
void get_packet(FILE *);
int next_char(FILE *);

char packet[PACKET_SIZE];

int main(void) {

  FILE *fp = fopen("6.txt", "r");
  int count = PACKET_SIZE;

  get_packet(fp);

  while (1) {
    if (!(check_packet(fp) && next_char(fp))) {
      break;
    }
    ++count;
  }

  printf("%d\n", count);

  return 0;
}

void get_packet(FILE *fp) {
  for (int i = 0; i < PACKET_SIZE; ++i) {
    packet[i] = fgetc(fp);
  }
}

int check_packet() {
  for (int i = 0; i < PACKET_SIZE-1; ++i) {
    for (int j = 1 + i; j < PACKET_SIZE; ++j) {
      if (packet[i] == packet[j]) {
        return 1;
      }
    }
  }
  return 0;
}

int next_char(FILE *fp) {
  for (int i = 0; i < PACKET_SIZE-1; ++i) {
    packet[i] = packet[i + 1];
  }
  packet[PACKET_SIZE-1] = getc(fp);
  if (packet[PACKET_SIZE-1] == '\n') {
    return 0;
  }
  return 1;
}
