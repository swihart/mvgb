#include "mvgb.h"

double F77_SUB(f77oneoversqrta)(int *n, double *p) {
  return(sqrt(qgamma(p[0], (double) n[0]/2.0, (double) 2.0/n[0], 0, 0)));
}


double F77_SUB(randomuniform)(void) {
  return unif_rand();
}


