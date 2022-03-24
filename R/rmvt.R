#' Multivariate t Distribution Random Variates
#'
#'
#' Computes random vectors of the multivariate t distribution
#' distribution for arbitrary correlation matrices using gamma variates
#' not chi/sqrt(nu).
#'
#'
#' @param n number of observations
#' @param df default to 30.
#' @param delta location vector.
#' @param Q semi-positive definite
#'
#' @references
#'
#' Genz, A. and Bretz, F. (2002), Methods for the computation of multivariate
#' t-probabilities. \emph{Journal of Computational and Graphical Statistics},
#' \bold{11}, 950--971.
#'
#'
#' @keywords distribution
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats   rgamma
#' @return Returns the \code{n} by \code{d} matrix containing multivariate t random variates
#' where \code{d=nrow(Q)}.
#' @examples
#' ## sample usage
#' rmvt(10, df=4, Q=diag(5))
#'
#' ## compare to rmvt from mvtnorm R-package
#' draw_mvtnorm <- mvtnorm::rmvt(2e4, sigma=diag(2), df = 35)
#' draw_mvgb    <-    mvgb::rmvt(2e4,     Q=diag(2), df = 35)
#'
#' rangex <- range(c(draw_mvgb[,1],draw_mvtnorm[,1]))
#' rangey <- range(c(draw_mvgb[,2],draw_mvtnorm[,2]))
#'
#' ## https://stackoverflow.com/a/53804499/2727349
#' opar <- par(mfrow=c(2,2))
#' plot(draw_mvtnorm, xlim=rangex, ylim=rangey); abline(h=0,v=0)
#' plot(draw_mvgb   , xlim=rangex, ylim=rangey); abline(h=0,v=0)
#'
#' hist(draw_mvtnorm, breaks = 100, ylim=c(0,4000), xlim=rangex)
#' hist(draw_mvgb   , breaks = 100, ylim=c(0,4000), xlim=rangex)
#' par(opar)
#'
#' @export
rmvt <- function(n, df=30, Q = NULL, delta=rep(0,d)
){
  d <- nrow(Q)

  A <- rgamma(n, shape=df/2, rate=df/2)

  G <-   mvtnorm::rmvnorm(n, sigma=Q)


  sqrt(1/A) * G + matrix(delta,nrow=n,ncol=length(delta),byrow=T)


}
