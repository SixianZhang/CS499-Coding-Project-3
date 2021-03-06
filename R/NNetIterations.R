#' Neural Network algorithm using iterations.
#'
#' This neural network algorithm has one output and one hidden layer, and stops when max.iterations is reached.
#'
#' @param X.mat numeric feature matrix of size [n_observations x n_features].
#' @param y.vec numeric label vector of length n_observations.
#' @param max.iterations integer scalar greater than 1.
#' @param step.size numeric positive scalar.
#' @param n.hidden.units number of hidden units, greater than or equal to 1.
#' @param is.train logical vector of length n_observations, TRUE if the observation is for training, FALSE for validation
#'
#' @return result.list with named elements:
#' pred.mat, n_observations x max.iterations matrix of predicted values.
#' V.mat final weight matrix (n_features+1 x n.hidden.units).
#' w.vec final weight vector (n.hidden.units+1).
#' predict(testX.mat) a function that takes a test features matrix and returns a vector of predictions.
#'
#' @export
#'
#' @examples
#' data(spam, package = "ElemStatLearn")
#' X.mat <- data.matrix(spam[,-ncol(spam)])
#' y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
#' max.iteration <- 50L
#' step.size <- 0.02
#' n.hidden.units = 20L
#' temp <- sample(rep(1:2,l=length(y.vec)))
#' is.train <- (temp == 1)
#' result.list <- NNetIterations(X.mat, y.vec, max.iteration, step.size, n.hidden.units, is.train = is.train)
NNetIterations <-
  function(X.mat,
           y.vec,
           max.iterations,
           step.size,
           n.hidden.units,
           is.train) {
    if (!all(is.matrix(X.mat), is.numeric(X.mat)))
      stop("The input matrix should be a numeric matrix")
    
    if (!all(is.numeric(y.vec),
             length(y.vec) == nrow(X.mat),
             is.vector(y.vec)))
      stop("The input y.vec should be a numeric vector with length nrow(X.mat)")
    
    if (!all(is.integer(max.iterations),
             max.iterations > 0,
             length(max.iterations) == 1))
      stop("max.iterations must be a integer scalar, which is greater than 0")
    
    if (!all(is.numeric(step.size), step.size > 0, length(step.size) == 1))
      stop("step.size must be numeric scalar, which is greater than 0")
    
    if (!all(is.integer(n.hidden.units),
             n.hidden.units > 0,
             length(n.hidden.units) == 1))
      stop("n.hidden.units must be a integer scalar, which is greater than 0")
    
    if (!all(is.vector(is.train),
             all(is.train %in% c(0, 1)),
             length(is.train) == length(y.vec)))
      stop("is.train must be a vector with either 0 or 1, with the same size with y.vec")
    
    if (all(y.vec %in% c(0, 1))) {
      is.binary <- 1
      y.vec <- ifelse(y.vec == 0, -1, 1)
    } else{
      is.binary <- 0
    }
    X.mat.old <- X.mat
    n.observations.total <- nrow(X.mat.old)
    X.mat <- X.mat[is.train,]
    y.train <- y.vec[is.train]
    n.observations <- nrow(X.mat)
    n.features <- ncol(X.mat)
    X.mean.vec <- colMeans(X.mat)
    
    X.std.vec <-
      sqrt(rowSums((t(X.mat) - X.mean.vec) ^ 2) / n.observations)
    X.std.vec[X.std.vec == 0] <- 1
    
    X.std.mat <- diag(n.features) * (1 / X.std.vec)
    
    X.scaled.mat <- t((t(X.mat) - X.mean.vec) / X.std.vec)
    X.scaled.train <- X.scaled.mat
    pred.mat <- matrix(0, n.observations.total, max.iterations)
    V.mat <- matrix(rnorm(n.features * n.hidden.units),
                    n.features,
                    n.hidden.units)
    
    w.vec <- rnorm(n.hidden.units)
    intercept.v <- matrix(rnorm(n.hidden.units), 1, n.hidden.units)
    intercept.w <- rnorm(1)
    b.mat <- matrix(0, n.observations, max.iterations)
    #    w.vec.iter <- runif(0,0.3)
    
    sigmoid <- function(x) {
      return(1 / (1 + exp(-x)))
    }
    
    dsigmoid <- function(x) {
      return(sigmoid(x) * (1 - sigmoid(x)))
    }
    
    for (iter.index in seq(max.iterations)) {
      temp.a.mat <-
        X.scaled.train %*% V.mat + rep(1, n.observations) %*% intercept.v  # n x u
      temp.z.mat <- sigmoid(temp.a.mat)  # n x u
      temp.b.vec <-
        temp.z.mat %*% w.vec + as.numeric(intercept.w)# n x 1
      if (is.binary) {
        ## temp.y.vec <- sigmoid(temp.b.vec)
        error <- sigmoid(-y.train * temp.b.vec) * (-y.train)
      } else{
        error <- temp.b.vec - y.train
      }
      w.vec <-
        w.vec - step.size * ((t(temp.z.mat) %*% (error)) / n.observations)
      V.mat <-
        V.mat - step.size * (t(X.scaled.train) %*% ((error) %*% t(w.vec) *
                                                      dsigmoid(temp.a.mat)) / n.observations)
      intercept.v <-
        intercept.v - step.size * (t(rep(1, n.observations)) %*% (error %*% t(w.vec)
                                                                  * dsigmoid(temp.a.mat))) / n.observations
      intercept.w <-
        intercept.w - step.size * (t(rep(1, n.observations)) %*% error) / n.observations
      
      V.mat.temp <-
        rbind(-X.mean.vec %*% X.std.mat %*% V.mat + intercept.v,
              X.std.mat %*% V.mat)
      w.vec.temp <- c(intercept.w, w.vec)
      
      if (is.binary) {
        pred.mat[, iter.index] <-
          sigmoid(cbind(1, sigmoid(cbind(1, X.mat.old) %*% V.mat.temp)) %*% w.vec.temp)
      } else
        pred.mat[, iter.index] <-
        cbind(1, sigmoid(cbind(1, X.mat.old) %*% V.mat.temp)) %*% w.vec.temp
    }
    V.mat <- V.mat.temp
    w.vec <- w.vec.temp
    
    result.list <- list(
      pred.mat = pred.mat,
      V.mat = V.mat.temp,
      w.vec = w.vec.temp,
      predict = function(testX.mat) {
        if(is.binary){
          prediction.vec <-
            sigmoid(cbind(1, sigmoid(cbind(1, testX.mat) %*% V.mat)) %*% w.vec)
        }else{
          prediction.vec <-
            cbind(1, sigmoid(cbind(1, testX.mat) %*% V.mat)) %*% w.vec
        }
        return(prediction.vec)
      }
    )
    return(result.list)
  }