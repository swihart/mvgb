#' Multivariate t Distribution Probabilities (via gamma variates)
#'
#'
#' Computes the the distribution function of the multivariate t
#' distribution for arbitrary limits, degrees of freedom and
#' correlation matrices.
#' This function is unlike \code{mvtnorm::pmvt} in two ways:
#'   1) The QRVSN method is used for every dimension \code{n} (including bivariate and trivariate),
#'   2) The QRVSN method on gamma variates, not chi/sqrt(nu).
#'
#' @param x NULL these are hardcoded for the moment
#' @param n the dimension.  Bivariate would be n=2, trivariate n=3, etc.
#' @param df degrees of freedom, sometimes referred to as nu
#' @param lower lower bounds of integration must be length n, finite
#' @param upper upper bounds of integration must be length n, finite
#' @param infin for finite intervals, must be vector length n every element 2
#' @param corr correlation
#' @param corrF correlationF
#' @param delta location vector must be length n
#'
#' @references
#'
#' Genz, A. and Bretz, F. (2002), Methods for the computation of multivariate
#' t-probabilities. \emph{Journal of Computational and Graphical Statistics},
#' \bold{11}, 950--971. <DOI:10.1198/106186002394>
#'
#' \url{http://www.math.wsu.edu/faculty/genz/homepage}
#'
#' \url{http://www.math.wsu.edu/faculty/genz/software/fort77/mvtdstpack.f}
#'
#' @keywords distribution
#' @return Returns the variables from the MVTDST function (QRVSN algorithm):
#'
#'     N      INTEGER, the number of variables.
#'
#'     NU     INTEGER, the number of degrees of freedom.
#'            If NU < 1, then an MVN probability is computed.
#'
#'     LOWER  DOUBLE PRECISION, array of lower integration limits.
#'
#'     UPPER  DOUBLE PRECISION, array of upper integration limits.
#'
#'     INFIN  INTEGER, array of integration limits flags:
#'             if INFIN(I) < 0, Ith limits are (-infinity, infinity);
#'             if INFIN(I) = 0, Ith limits are (-infinity, UPPER(I)];
#'             if INFIN(I) = 1, Ith limits are [LOWER(I), infinity);
#'             if INFIN(I) = 2, Ith limits are [LOWER(I), UPPER(I)].
#'
#'     CORREL DOUBLE PRECISION, array of correlation coefficients;
#'            the correlation coefficient in row I column J of the
#'            correlation matrix should be stored in
#'               CORREL( J + ((I-2)*(I-1))/2 ), for J < I.
#'            The correlation matrix must be positive semi-definite.
#'
#'     DELTA  DOUBLE PRECISION, array of non-centrality parameters.
#'
#'     MAXPTS INTEGER, maximum number of function values allowed. This
#'            parameter can be used to limit the time. A sensible
#'            strategy is to start with MAXPTS = 1000*N, and then
#'            increase MAXPTS if ERROR is too large.
#'
#'     ABSEPS DOUBLE PRECISION absolute error tolerance.
#'
#'     RELEPS DOUBLE PRECISION relative error tolerance.
#'
#'     error  DOUBLE PRECISION estimated absolute error,
#'            with 99% confidence level.
#'
#'     value  DOUBLE PRECISION estimated value for the integral
#'
#'     inform INTEGER, termination status parameter:
#'            if INFORM = 0, normal completion with ERROR < EPS;
#'            if INFORM = 1, completion with ERROR > EPS and MAXPTS
#'                           function values used; increase MAXPTS to
#'                           decrease ERROR;
#'            if INFORM = 2, N > 1000 or N < 1.
#'            if INFORM = 3, correlation matrix not positive semi-definite.
#'
#'     RND    ignore; this initializes RNG
#'
#' @examples
#'     mvgb::pmvt(x=NULL,
#'                n=5,
#'                df=4,
#'                lower=rep(-1,5),
#'                upper=rep(1,5),
#'                infin=rep(2,5),
#'                corr=structure(c(1, 0.85, 0.85, 0.85, 0.85,
#'                                 0, 1   , 0.85, 0.85, 0.85,
#'                                 0, 0   , 1   , 0.85, 0.85,
#'                                 0, 0   , 0   , 1   , 0.85,
#'                                 0, 0   , 0   , 0   , 1   ),
#'                  .Dim = c(5L,5L)),
#'                 corrF=NULL,
#'                 delta=rep(.1,5))
#' @export
pmvt <- function(x, n, df, lower, upper, infin, corr, corrF, delta) {

  if (n > 1) {
    corrF <- matrix(as.vector(corr), ncol=n, byrow=TRUE)
    corrF <- corrF[upper.tri(corrF)]
  } else corrF <- corr

  error <- 0; value <- 0; inform <- 0
  .C(C_mvtdst,
     N = as.integer(n),
     NU = as.integer(df),
     LOWER = as.double(lower),
     UPPER = as.double(upper),
     INFIN = as.integer(infin),
     CORREL = as.double(corrF),
     DELTA = as.double(delta),
     MAXPTS = as.integer(25000*10),##as.integer(25000*100),##as.integer(25000),
     ABSEPS = as.double(0.0001),##as.double(0.001),
     RELEPS = as.double(0),
     error = as.double(error),
     value = as.double(value),
     inform = as.integer(inform),
     RND = as.integer(1)) ### init RNG
}
