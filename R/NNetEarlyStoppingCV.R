#' Neral Network algorithm using cross validation
#'
#' This neral network algorithm has one output and one hidden layer, and uses cross validation to find the best step
#' size corresponding to the max.iterations.
#'
#' @param X.mat numeric feature matrix of size [n_observations x n_features].
#' @param y.vec numeric label vector of length n_observations.
#' @param fold.vec numeric fold vector of length n_observations.
#' @param max.iterations integer scalar greater than 1.
#' @param step.size numeric positive scalar.
#' @param n.hidden.units number of hidden units, greater than or equal to 1.
#'
#' @return result.list with named elements:
#' pred.mat, n_observations x max.iterations matrix of predicted values.
#' W.mat final weight matrix (n_features+1 x n.hidden.units).
#' v.vec final weight vector (n.hidden.units+1).
#' mean.validation.loss.vec mean loss for all validation sets.
#' mean.train.loss.vec mean loss for all training sets.
#' selected steps best step size selected corresponding to the max.iterations.
#' predict(testX.mat) a function that takes a test features matrix and returns a vector of predictions.
#'
#' @export
#'
#' @examples
NNetEarlyStoppingCV <-
  function(X.mat,
           y.vec,
           fold.vec,
           max.iterations,
           step.size,
           n.hidden.units) {
    # Check data
    
    if (!all(is.numeric(X.mat), is.matrix(X.mat))) {
      stop("X.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of length nrow(X.mat)")
    }
    
    if (is.null(fold.vec)) {
      fold.vec <- sample(rep(1:5, l = nrow(X.mat)))
    } else{
      if (!all(is.numeric(fold.vec),
               is.vector(fold.vec),
               length(fold.vec) == nrow(X.mat))) {
        stop("fold.vec must be a numeric vector of length nrow(X.mat)")
      }
    }
    
    if (!all(
      is.numeric(max.iterations),
      is.integer(max.iterations),
      max.iterations > 1,
      length(max.iterations) == 1
    )) {
      stop("max.iterations must be an integer scalar greater than 1")
    }
    
    if (!all(is.numeric(step.size), length(step.size) == 1, step.size >= 0)) {
      stop("step.size must be a positive scalar")
    }
    
    if (!all(
      is.numeric(n.hidden.units),
      is.integer(n.hidden.units),
      length(n.hidden.units) == 1,
      n.hidden.units >= 1
    )) {
      stop("n.hidden.units must be an integer scalar greater than or equeal to 1")
    }
    
    
    
    
    result.list <- list(
      pred.mat = ,
      W.mat = ,
      v.vec = ,
      mean.validation.loss.vec = ,
      mean.train.loss.vec = ,
      selected.steps = ,
      predict = function(testX.mat) {
        prediction.vec <- sigmoid(cbind(1, testX.mat) %*% W.mat) %*% v.vec
        
        
        return(prediction.vec)
        
      }
    )
    
    return(result.list)
    
  }