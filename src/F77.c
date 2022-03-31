#include "mvgb.h"

double F77_SUB(f77oneoversqrta_where_a_is_inverse_gamma)(int *n, double *p) {
  return(sqrt(qgamma(p[0], (double) n[0]/2.0, (double) 2.0/n[0], 0, 0)));
  //return(sqrt(rgamma((double) n[0]/2.0, (double) 2.0/n[0])));
}

double F77_SUB(f77oneoversqrta_where_a_is_exponential)(int *n, double *p) {
  return(sqrt(1/qexp(p[0], 1.0, 0, 0)));
  //return(sqrt(1/rexp(1.0)));
}

// The only downside of the approach below is that it requires a substantially
//   larger MAXPTS to get an inform = 0 returned.  But this seems less hacky
//   than fabs() or rerun.nan solutions of the past...(?)
double F77_SUB(f77oneoversqrta_where_a_is_posstab)(int *n, double *p) {

  double input2 = ((double) (n[0]-900000000)/1000000.0)/2.0;  //0 // // // // //

  double k1 = p[0]*PI;//runif(0.0, PI);//p[0]*PI;
  double k2 = rexp(1);//qexp(p[0], 1.0, 0, 0);//rexp(p[0]/p[0]);

  /* generate u_i using k1, k2*/
  double A = sin(input2 * k1) /pow( sin(k1),(1/input2)    ) * pow(sin((1-input2)*k1)/k2, (1/input2-1));

  double sqrtA = sqrt(2*A);
  return(1.0/sqrtA);
}

double F77_SUB(randomuniform)(void) {
  return unif_rand();
}


