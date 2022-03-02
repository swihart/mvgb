#include "mvgb.h"
#include <R_ext/Rdynload.h>

void C_mvtdst(int *n, int *nu, double *lower, double *upper,
              int *infin, double *corr, double *delta,
              int *maxpts, double *abseps, double *releps,
              double *error, double *value, int *inform, int *rnd)
{

    if (rnd[0]) GetRNGstate();

    /* call FORTRAN subroutine */
    F77_CALL(mvtdst)(n, nu, lower, upper,
                     infin, corr, delta,
                     maxpts, abseps, releps,
                     error, value, inform);

    if (rnd[0]) PutRNGstate();

}

static const R_CMethodDef cMethods[] = {
    {"C_mvtdst", (DL_FUNC) &C_mvtdst, 14, (R_NativePrimitiveArgType[14]){INTSXP,
					   INTSXP, REALSXP, REALSXP,
                                           INTSXP, REALSXP, REALSXP,
                                           INTSXP, REALSXP, REALSXP,
                                           REALSXP, REALSXP, INTSXP, INTSXP}},
    {NULL, NULL, 0}
};


void R_init_mvgb(DllInfo *dll)
{
    R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_RegisterCCallable("mvgb", "C_mvtdst", (DL_FUNC) &C_mvtdst);
}
