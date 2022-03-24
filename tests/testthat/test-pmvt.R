test_that("mvgb::pmvt is similar to mvtnorm::pmvt", {
  expect_equal(

    {set.seed(1);mvgb::pmvt(x=NULL,
               n=5,
               df=4,
               lower=rep(-1,5),
               upper=rep(1,5),
               infin=rep(2,5),
               corr=structure(c(1, 0.85, 0.85, 0.85, 0.85, 0, 1, 0.85, 0.85, 0.85,
                                0, 0, 1, 0.85, 0.85, 0, 0, 0, 1, 0.85, 0, 0, 0, 0, 1), .Dim = c(5L,
                                                                                                5L)),
               corrF=NULL,
               delta=rep(.1,5))$value},

    {set.seed(1);mvtnorm::pmvt(lower=rep(-1,5), upper=rep(1,5), delta=rep(.1,5), df=4, corr=structure(c(1, 0.85, 0.85, 0.85, 0.85, 0, 1, 0.85, 0.85, 0.85,
                                                                                           0, 0, 1, 0.85, 0.85, 0, 0, 0, 1, 0.85, 0, 0, 0, 0, 1), .Dim = c(5L,
                                                                                                                                                           5L)))[1]},


    tolerance = 1e-3
  )
})
