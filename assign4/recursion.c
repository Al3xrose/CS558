// Alex Rose
//-------------------------------------------------------------------------

// Regular vs tail recursive functions
//
#include "stdio.h"

// factorial function

int fac(int n) {
  if (n <= 1) 
    return 1;
  return n * fac(n-1);
}

int facTR(int n) {
  int helper(int k, int acc) {
    if (k <= 1)
      return acc;
    return helper(k-1, k*acc);
  }
  return helper(n, 1);
}

// unknown function g

int g(int n) {
  if (n <= 3) 
    return 1;
  else return g(n-1) + g(n-2) + g(n-3);
}

int gTR(int n) {
  int helper(int n, int step, int x1, int x2, int x3){
    if (step == n){
      return x1;
    }
    return helper(n, step+1, x1+x2+x3, x1, x2);
  };

  if(n <= 3){
    return 1;
  }
  else
    return helper(n, 3, 1, 1, 1);
}

// unknown function h

int h(int n) {
  if (n <= 1)
    return 1;
  if (n%2 == 0) 
    return h(n/2) + n;
  else
    return h(n-1) + 1;
}

int hTR(int n) {
  int helper(int k, int acc)
  {
    if(k <= 1)
      return acc+1;
    else if(k%2 == 0)
      return helper(k/2, acc+k);
    else
      return helper(k-1, acc+1);
  };

  if(n <= 1)
    return 1;
  else
    return helper(n, 0);
}

int main() {
  printf("fac(1-10)   = ");
  for (int i=1; i<10; i++) printf("%d, ", fac(i));
  printf("%d\n", fac(10));

  printf("facTR(1-10) = ");
  for (int i=1; i<10; i++) printf("%d, ", facTR(i));
  printf("%d\n", facTR(10));

  printf("g(1-10)   = ");
  for (int i=1; i<10; i++) printf("%d, ", g(i));
  printf("%d\n", g(10));

  printf("gTR(1-10) = ");
  for (int i=1; i<10; i++) printf("%d, ", gTR(i));
  printf("%d\n", gTR(10));

  printf("h(1-10)   = ");
  for (int i=1; i<10; i++) printf("%d, ", h(i));
  printf("%d\n", h(10));

  printf("hTR(1-10) = ");
  for (int i=1; i<10; i++) printf("%d, ", hTR(i));
  printf("%d\n", hTR(10));
}
