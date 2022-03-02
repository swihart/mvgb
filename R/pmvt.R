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
     MAXPTS = as.integer(25000),
     ABSEPS = as.double(0.001),
     RELEPS = as.double(0),
     error = as.double(error),
     value = as.double(value),
     inform = as.integer(inform),
     RND = as.integer(1)) ### init RNG
}
