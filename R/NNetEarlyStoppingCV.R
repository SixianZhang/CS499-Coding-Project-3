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
#' selected steps best step size selected corresponding to the minimum mean validation loss.
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
    
    # Initialize
    
    is.binary <- ifelse((all(y.vec %in% c(0,1))), TRUE, FALSE)
    
    n.fold <- length(unique(fold.vec))
    n.feature <- ncol(X.mat)
    n.observation <- nrow(X.mat)
    validation.loss.mat <-
      matrix(0, nrow = n.fold, ncol = max.iterations)
    train.loss.mat <-
      matrix(0, nrow = n.fold, ncol = max.iterations)
    
    sigmoid <- function(x){
      1/(1 + exp(-x))
    }
   
    # iterate through each folds
    for (i.fold in seq(n.fold)) {
      train.index <- which(fold.vec != i.fold)
      validation.index <- which(fold.vec == i.fold)

      model.list <-
        NNetIteration(X.mat[train.index, ],
                      y.vec[train.index],
                      max.iterations,
                      step.size,
                      n.hidden.units,
                      TRUE)
      
      v.vec <- model.list$v.vec
      W.mat <- model.list$W.mat
      
      train.predict <- cbind(1, sigmoid(cbind(1,X.mat[train.index]) %*% W.mat)) %*% v.vec
      if(is.binary){
        # Do 0-1 loss
      }else{
        # Do square loss
      }
      
      validation.predict <- cbind(1, sigmoid(cbind(1,X.mat[validation.index]) %*% W.mat)) %*% v.vec
      if(is.binary){
        # Do 0-1 loss
      }else{
        # Do square loss
      }
    }
    
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    mean.train.loss.vec <- colMeans(train.loss.mat)
    
    selected.steps <- which.min(mean.train.loss.vec)
    
    selected.list <- NNetIteration(X.mat, y.vec, selected.steps, step.size, n.hidden.units, TRUE)
    
    result.list <- list(
      pred.mat = selected.list$pred.mat,
      W.mat = selected.list$W.mat,
      v.vec = selected.list$v.vec,
      mean.validation.loss.vec = mean.validation.loss.vec,
      mean.train.loss.vec = mean.train.loss.vec,
      selected.steps = selected.steps,
      predict = function(testX.mat) {
        prediction.vec <- sigmoid(cbind(1, testX.mat) %*% W.mat) %*% v.vec
        
        
        return(prediction.vec)
        
      }
    )
    
    return(result.list)
    
  }