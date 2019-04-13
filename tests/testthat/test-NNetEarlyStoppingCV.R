library(testthat)
library(NeuralNetwork)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
max.iteration <- 5L
step.size <- 0.5
n.hidden.units <- 200L
temp <- sample(rep(1:3,l=length(y.vec)))

# LMLogisticLossIterations X.mat, y.vec, max.iterations, step.size = 0.5

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    result.list <-
      NNetEarlyStoppingCV(X.mat, y.vec, max.iterations=max.iteration, step.size=step.size, n.hidden.units=n.hidden.units)
    expect_true(is.list(result.list))
    expect_true(is.matrix(result.list$V.mat))
    expect_equal(nrow(result.list$V.mat), ncol(cbind(1,X.mat)))
    expect_equal(length(result.list$w.vec), n.hidden.units + 1)
    expect_true(is.vector(result.list$mean.validation.loss.vec))
    expect_true(is.vector(result.list$mean.train.loss.vec))
    expect_equal(length(result.list$mean.train.loss.vec),max.iteration)
    expect_equal(length(result.list$mean.validation.loss.vec),max.iteration)
  }
)



test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      result.list <- 
        NNetEarlyStoppingCV(as.data.frame(X.mat), y.vec, max.iterations=max.iteration, step.size=step.size, n.hidden.units=n.hidden.units),
      "X.mat must be a numeric matrix",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetEarlyStoppingCV(X.mat, c(1,y.vec), max.iterations=max.iteration, step.size=step.size, n.hidden.units=n.hidden.units),
      "y.vec must be a numeric vector of length nrow(X.mat)",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetEarlyStoppingCV(X.mat, y.vec, max.iterations=as.double(max.iteration), step.size=step.size, n.hidden.units=n.hidden.units),
      "max.iterations must be an integer scalar greater than 1",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetEarlyStoppingCV(X.mat, y.vec, max.iterations=max.iteration, step.size=c(1,step.size), n.hidden.units=n.hidden.units),
      "step.size must be a positive scalar",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetEarlyStoppingCV(X.mat, y.vec, max.iterations=max.iteration, step.size=step.size, n.hidden.units=as.double(n.hidden.units)),
      "n.hidden.units must be an integer scalar greater than or equeal to 1",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetEarlyStoppingCV(X.mat, y.vec, max.iterations=max.iteration, step.size=step.size, n.hidden.units=n.hidden.units, n.folds=1L),
      "n.folds must be a positive integer scalar",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetEarlyStoppingCV(X.mat, y.vec, fold.vec=temp, max.iterations=max.iteration, step.size=step.size, n.hidden.units=n.hidden.units, n.folds=2L),
      "fold.vec must be a numeric vector of length nrow(X.mat) with n.folds unique elements",
      fixed = TRUE
    )   
    
  }
)