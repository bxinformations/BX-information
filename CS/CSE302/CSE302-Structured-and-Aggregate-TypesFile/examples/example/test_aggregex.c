#include <stdio.h>
#include <stdint.h>

extern int64_t* dynarr_ptwise(int64_t *x, int64_t *y);
extern int64_t* vec3_ptwise(int64_t *x, int64_t *y);
extern int64_t vec3_dot(int64_t *x, int64_t *y);

int main()
{
  int64_t x[3] = { 1, 2, 3 };
  int64_t y[3] = { 300, 200, 100 };
  int64_t *z1 = dynarr_ptwise(x, y);
  printf("z1 = [%ld, %ld, %ld]\n", z1[0], z1[1], z1[2]);
  int64_t *z2 = vec3_ptwise(x, y);
  printf("z2 = [%ld, %ld, %ld]\n", z2[0], z2[1], z2[2]);
  int64_t z3 = vec3_dot(x, y);
  printf("z3 = %ld\n", z3);
}
