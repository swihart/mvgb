#include "mvgb.h"

double F77_SUB(f77oneoversqrta_where_a_is_inverse_gamma)(int *n, double *p) {
  return(sqrt(qgamma(p[0], (double) n[0]/2.0, (double) 2.0/n[0], 0, 0)));
  //return(sqrt(rgamma((double) n[0]/2.0, (double) 2.0/n[0])));
}

double F77_SUB(f77oneoversqrta_where_a_is_exponential)(int *n, double *p) {
  return(sqrt(1/qexp(p[0], 1.0, 0, 0)));
  //return(sqrt(1/rexp(1.0)));
}

double F77_SUB(f77oneoversqrta_where_a_is_posstab)(int *n, double *p) {

  double alpha = ((double) (n[0]-900000000)/1000000.0)/2.0;  //1 // // // // //

  return(sqrt(qgamma(p[0], (double) alpha, (double) 1/alpha, 0, 0)));




  }


double F77_SUB(randomuniform)(void) {
  return unif_rand();
}


