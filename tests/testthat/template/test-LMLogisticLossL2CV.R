library(testthat)
library(LinearModel)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
fold.vec <- sample(rep(1:4, l = length(y.vec)))
penalty.vec <- seq(5, 0.1, by = -0.1)
# LMLogisticLossL2CV X.mat, y.vec, fold.vec, penalty.vec

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    result.list <-
      LMLogisticLossL2CV(X.mat, y.vec, fold.vec, penalty.vec)
    expect_true(is.list(result.list))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      result.list <- 
        LMLogisticLossL2CV(as.data.frame(X.mat), y.vec, fold.vec, penalty.vec),
      "X.mat must be a numeric matrix",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossL2CV(X.mat, y.vec[-1], fold.vec, penalty.vec),
      "y.vec must be a numeric vector of length nrow(X.mat)",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossL2CV(X.mat, y.vec, fold.vec[-1], penalty.vec),
      "fold.vec must be a numeric vector of length nrow(X.mat)",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossL2CV(X.mat, y.vec, fold.vec, seq(0.1, 5, by = 0.1)),
      "penalty.vec must be a non-negative decreasing numeric vector",
      fixed = TRUE
    )
  }
)

