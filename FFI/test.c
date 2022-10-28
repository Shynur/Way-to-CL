#include <stdio.h>
#include <stdlib.h>

struct c_struct {
  int x;
  char *s;
};

struct c_struct *c_function(int i, char *s, struct c_struct *r, int a[10]) {
  int j;
  struct c_struct *r2;

  printf("i    = %d\n", i);
  printf("s    = %s\n", s);
  printf("r->x = %d\n", r->x);
  printf("r->s = %s\n", r->s);
  
  for (j = 0; j < 10; ++j)
      printf("a[%d] = %d.\n", j, a[j]);
  
  r2 = (struct c_struct *)malloc(sizeof(struct c_struct));
  r2->x = i + 5;
  r2->s = "a C string";

  return r2;
};
