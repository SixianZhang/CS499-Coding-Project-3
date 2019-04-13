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
#' @param n.folds positive integer scalar, numbers of folds, default is 4
#'
#' @return result.list with named elements:
#' pred.mat, n_observations x max.iterations matrix of predicted values.
#' V.mat final weight matrix (n_features+1 x n.hidden.units).
#' w.vec final weight vector (n.hidden.units+1).
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
           fold.vec = sample(rep(1:n.folds, l = length(y.vec))),
           max.iterations,
           step.size,
           n.hidden.units,
           n.folds = 4L) {
    # Check data
    
    if (!all(is.numeric(X.mat), is.matrix(X.mat))) {
      stop("X.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of length nrow(X.mat)")
    }
    

    if (!all(is.numeric(fold.vec),
             is.vector(fold.vec),
             length(unique(fold.vec)) == n.folds,
             length(fold.vec) == nrow(X.mat))) {
      stop("fold.vec must be a numeric vector of length nrow(X.mat) with n.folds unique elements")
    }

    if (!all(
      is.numeric(max.iterations),
      is.integer(max.iterations),
      length(max.iterations) == 1,
      max.iterations > 1
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
    
    if(!all(
      is.numeric(n.folds),
      is.integer(n.folds),
      n.folds > 0, 
      length(n.folds) == 1
    )){
      stop("n.folds must be a positive integer scalar")
    }
    
    # Initialize
    
    is.binary <- ifelse((all(y.vec %in% c(-1,1))), TRUE, FALSE)
    
    n.folds <- length(unique(fold.vec))
    n.feature <- ncol(X.mat)
    n.observation <- nrow(X.mat)
    validation.loss.mat <-
      matrix(0, nrow = n.folds, ncol = max.iterations)
    train.loss.mat <-
      matrix(0, nrow = n.folds, ncol = max.iterations)
    
    sigmoid <- function(x){
      1/(1 + exp(-x))
    }
   
    # iterate through each folds
    for (i.fold in seq(n.folds)) {
      train.vec <- (fold.vec != i.fold)
      
      model.list <-
        NNetIterations(X.mat=X.mat,
                      y.vec=y.vec,
                      max.iterations=max.iterations,
                      step.size=step.size,
                      n.hidden.units=n.hidden.units,
                      is.train = train.vec)
      
      w.vec <- model.list$w.vec
      V.mat <- model.list$V.mat
     
      set.list <- list(train = train.vec, validation = (!train.vec))
      for(set.name in names(set.list)){
        predict <- model.list$pred.mat[get(set.name,set.list),]
        
        if(is.binary){
          # Do 0-1 loss
          predict <- ifelse(predict > 0.5, 1, -1)
          loss.vec <- colMeans((ifelse(predict == y.vec[get(set.name,set.list)], 0, 1)))
        }else{
          # Do square loss
          loss.vec <- colMeans((predict - y.vec[get(set.name,set.list)])^2)
        }
        
        if(set.name == "train"){
          train.loss.mat[i.fold,] <- loss.vec
        }else{
          validation.loss.mat[i.fold,] <- loss.vec
        }
      }
      
      # train.predict <- model$pred.mat[train.index,]
      # if(is.binary){
      #   # Do 0-1 loss
      #   train.predict <- ifelse(train.predict > 0.5, 1, 0)
      #   train.loss.mat[i.fold,] <- colMeans((ifelse(train.predict == y.vec[train.index], 0, 1)))
      # }else{
      #   # Do square loss
      #   train.loss.mat[i.fold,] <- colMeans((train.predict - y.vec[train.index])^2)
      # }
      # 
      # validation.predict <- model$pred.mat[validation.index,]      
      # if(is.binary){
      #   # Do 0-1 loss
      #   validation.predict <- ifelse(validation.predict > 0.5, 1, 0)
      #   validation.loss.mat <- colMeans(ifelse(validation.predict == y.vec[validation.index], 0, 1))
      # }else{
      #   # Do square loss
      #   validation.loss.mat[i.fold,] <- colMeans((validation.predict - y.vec[validation.index])^2)
      # }
    }
    
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    mean.train.loss.vec <- colMeans(train.loss.mat)
    selected.steps <- which.min(mean.validation.loss.vec)
    
    result.list <- NNetIterations(X.mat, y.vec, selected.steps, step.size, n.hidden.units, rep(TRUE,n.observation))
    
    # result.list <- list(
    #   pred.mat = selected.list$pred.mat,
    #   V.mat = selected.list$V.mat,
    #   w.vec = selected.list$w.vec,
    #   mean.validation.loss.vec = mean.validation.loss.vec,
    #   mean.train.loss.vec = mean.train.loss.vec,
    #   selected.steps = selected.steps,
    # 
    #   predict = function(testX.mat) {
    #     prediction.vec <- sigmoid(cbind(1, testX.mat) %*% V.mat) %*% w.vec
    #     
    #     
    #     return(prediction.vec)
    #     
    #   }
    # )
    
    result.list$mean.validation.loss.vec = mean.validation.loss.vec
    result.list$mean.train.loss.vec = mean.train.loss.vec
    result.list$selected.steps = selected.steps
    
    
    return(result.list)
    
  }