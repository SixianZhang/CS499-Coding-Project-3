#' Neral Network algorithm using cross validation
#' 
#' This neral network algorithm has one output and one hidden layer, and uses cross validation to find the best step
#' size corresponding to the max.iterations.
#'
#' @param X.mat numeric feature matrix of size [n_observations x n_features]. 
#' @param y.vec numeric label vector of length n_observations.
#' @param fold.vec numeric fold vector of length n_observations.
#' @param max.iterations integer scalar greater than 1.
#' @param step.size numeric non-negative scalar.
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
NNetEarlyStoppingCV <- function(X.mat, y.vec, fold.vec, max.iterations, step.size, n.hidden.units){
  
  result.list <- list(pred.mat = ,
                      W.mat = , 
                      v.vec = , 
                      mean.validation.loss.vec = ,
                      mean.train.loss.vec = ,
                      selected.steps = ,
                      predict = function(testX.mat){
                        
                        prediction.vec <- sigmoid(cbind(1,testX.mat)%*%W.mat)%*%v.vec;
                        
                        return prediction.vec;
                      })
  
  return result.list;
}