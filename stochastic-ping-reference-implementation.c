#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

/* The ping formula should have the exact same behavior matching this
 * reference implementation in every tt related tool. It's possible to use
 * different programs or devices for generating the ping notifications and for
 * logging action at the time of ping, so the implementations need to agree on
 * the timing of the pings.
 */

int is_ping(int64_t avg_secs, int64_t t) {
  /* This is the xorshift64 algorithm */
  t ^= t << 13;
  t ^= t >> 7;
  t ^= t << 17;
  return (t % avg_secs) == 0;
}

/* Usage example, not part of reference implementation. */

int main(int argc, char *argv[]) {
  if (argc < 3) {
    printf(
      "Usage: %s [ping interval in minutes] [current unix time]\n",
      argv[0]);
    printf(
      "  Displays seconds to wait until next ping.\n");
    return 1;
  }

  int64_t avg_minutes = atoi(argv[1]);
  int64_t unix_time = atoi(argv[2]);
  int64_t ping_time = unix_time;

  while (!is_ping(avg_minutes * 60, ++ping_time)) {}

  printf("%ld\n", ping_time - unix_time);
}
