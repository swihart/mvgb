#' Multivariate Subgaussian Stable Distribution Probabilities (via positive stable variates)
#'
#'
#' Computes the the distribution function of the multivariate subgaussian stable
#' distribution for arbitrary limits, alpha,
#' shape matrices, and location vectors.
#' This function is unlike \code{mvtnorm::pmvt} in two ways:
#'   1) The QRVSN method is used for every dimension \code{n} (including bivariate and trivariate),
#'   2) The QRVSN method on positive stable variates, not chi/sqrt(nu).
#'
#'
#' @param lower lower bounds of integration must be length n, finite
#' @param upper upper bounds of integration must be length n, finite
#' @param alpha real number between 0 and 2 that cannot have more than 6 digits precision.
#'  0.123456 is okay; 0.1234567 is not. Thus alpha in \[0.000001, 1.999999\].
#' @param Q shape matrix
#' @param delta location vector must have length equal to the number of rows of Q. Defaults to the 0 vector.
#' @param maxpts (description from FORTRAN code) INTEGER, maximum number of function values allowed. This
#' parameter can be used to limit the time. A sensible
#' strategy is to start with MAXPTS = 1000*N, and then
#' increase MAXPTS if ERROR is too large. (description from mvtnorm::GenzBretz) maximum number of function values as integer. The internal FORTRAN code always uses a minimum number depending on the dimension. (for example 752 for three-dimensional problems).
#' @param abseps absolute error tolerance
#' @param releps relative error tolerance as double.
#'
#' @references
#'
#' Genz, A. and Bretz, F. (2002), Methods for the computation of multivariate
#' t-probabilities. \emph{Journal of Computational and Graphical Statistics},
#' \bold{11}, 950--971.
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
#'Q = structure(c(1, 0.85, 0.85, 0.85, 0.85,
#'                0.85, 1   , 0.85, 0.85, 0.85,
#'                0.85, 0.85   , 1   , 0.85, 0.85,
#'                0.85, 0.85   , 0.85   , 1   , 0.85,
#'                0.85, 0.85   , 0.85   , 0.85   , 1   ),
#'              .Dim = c(5L,5L))
#'
#'## default maxpts=25000 doesn't finish with error < abseps
#'mvgb::pmvss(lower=rep(-1,5),
#'            upper=rep(1,5),
#'            alpha=1,
#'            Q=Q,
#'            maxpts=25000)[c("value","inform","error")]
#'
#'## increase maxpts to get inform value 0 (that is, error < abseps)
#'mvgb::pmvss(lower=rep(-1,5),
#'            upper=rep(1,5),
#'            alpha=1,
#'            Q=Q,
#'            maxpts=25000*30)[c("value","inform","error")]
#'
#'
#'set.seed(10)
#'shape_matrix <- structure(c(1, 0.9, 0.9, 0.9, 0.9, 0.9, 1, 0.9, 0.9, 0.9, 0.9,
#'                            0.9, 1, 0.9, 0.9, 0.9, 0.9, 0.9, 1, 0.9, 0.9, 0.9, 0.9, 0.9,
#'                            1), .Dim = c(5L, 5L))
#'
#'mvgb::pmvss(lower=rep(-2,5),
#'            upper=rep(2,5),
#'            alpha=1.7,
#'            Q=shape_matrix,
#'            delta=rep(0,5),
#'            maxpts=25000*30)[c("value","inform","error")]
#' @importFrom stats cov2cor
#' @export
pmvss <- function(lower, upper, alpha, Q, delta=rep(0,NROW(Q)), maxpts=25000, abseps = 0.001, releps = 0) {

  #########################
  ## Ascertain dimension ##
  #########################
  n=NROW(Q)


  #########################
  ## Stop out of bounds  ##
  #########################
  if (n<1 | n>1000) stop("only dimensions 1 <= n <= 1000 allowed")


  if(alpha < 0.000001 | alpha > 1.999999)
    stop("\n`alpha` is out of bounds.\nPlease make sure alpha is in [0.000001, 1.999999]")


  ###############################
  ## Prep the infinite limits  ##
  ###############################
  is_pos_inf <- function(w) is.infinite(w) & w > 0
  is_neg_inf <- function(w) is.infinite(w) & w < 0

  infin <- rep(2, n)
  infin[is_pos_inf(upper)] <- 1
  infin[is_neg_inf(lower)] <- 0
  infin[is_neg_inf(lower) & is_pos_inf(upper)] <- -1

  upper[infin %in% c(-1,1)] <- 0
  lower[infin %in% c(-1,0)] <- 0


  ############################################
  ## Take care of the all -Inf to Inf case  ##
  ############################################
  if (all(infin < 0))
    return(list(value = 1, error = 0))


  ##################################################
  ## Adjust to correlation input; subtract delta  ##
  ##################################################

  sqd <- sqrt(diag(Q))
  lower <- (lower-delta)/sqd
  upper <- (upper-delta)/sqd
  corr <- cov2cor(Q)

  ##################################################
  ## Submit just upper triangle of corr           ##
  ##################################################

  if (n > 1) {
    corrF <- matrix(as.vector(corr), ncol=n, byrow=TRUE)
    corrF <- corrF[upper.tri(corrF)]
  } else corrF <- corr

  error <- 0; value <- 0; inform <- 0
  .C(C_mvtdst,
     N = as.integer(n),
     NU = as.integer(900000000 + (1000000 * alpha)),
     LOWER = as.double(lower),
     UPPER = as.double(upper),
     INFIN = as.integer(infin),
     CORREL = as.double(corrF),
     DELTA = as.double(rep(0,n)),## fix at 0; delta for pmvt is diff than pmvss
     MAXPTS = as.integer(maxpts),
     ABSEPS = as.double(abseps),
     RELEPS = as.double(releps),
     error = as.double(error),
     value = as.double(value),
     inform = as.integer(inform),
     RND = as.integer(1)) ### init RNG
}
