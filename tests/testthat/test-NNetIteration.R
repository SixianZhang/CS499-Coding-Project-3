library(testthat)
library(NeuralNetwork)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
max.iteration <- 50L
step.size <- 0.02
n.hidden.units = 20L
temp <- sample(rep(1:2,l=length(y.vec)))
is.train <- (temp == 1)
# LMLogisticLossIterations X.mat, y.vec, max.iterations, step.size = 0.5

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    result.list <-
      NNetIterations(X.mat, y.vec, max.iteration, step.size, n.hidden.units, is.train = is.train)
    expect_true(is.list(result.list))
    expect_true(is.matrix(result.list$V.mat))
    expect_equal(nrow(result.list$V.mat), ncol(cbind(1,X.mat)))
    expect_equal(length(result.list$w.vec), n.hidden.units + 1)
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      result.list <- 
        NNetIterations(as.data.frame(X.mat), y.vec, max.iteration, step.size, n.hidden.units, is.train),
      "The input matrix should be a numeric matrix",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetIterations(X.mat, y.vec[-1], max.iteration, step.size, n.hidden.units, is.train),
      "The input y.vec should be a numeric vector with length nrow(X.mat)",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetIterations(X.mat, y.vec, as.double(max.iteration), step.size, n.hidden.units, is.train),
      "max.iterations must be a integer scalar, which is greater than 0",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        NNetIterations(X.mat, y.vec, max.iteration, c(rep(step.size,2)), n.hidden.units, is.train),
      "step.size must be numeric scalar, which is greater than 0",
      fixed = TRUE
    )
    
    expect_error(
      result.list <-
        NNetIterations(X.mat, y.vec, max.iteration, step.size, as.double(n.hidden.units), is.train),
      "n.hidden.units must be a integer scalar, which is greater than 0",
      fixed = TRUE
    )
    
    expect_error(
      result.list <- 
        NNetIterations(X.mat, y.vec, max.iteration, step.size, n.hidden.units, c(2,is.train)),
      "is.train must be a vector with either 0 or 1, with the same size with y.vec",
      fixed = TRUE
    )
  
  }
)