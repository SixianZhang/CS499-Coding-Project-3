#' Neural Network algorithm using iterations.
#' 
#' This neural network algorithm has one output and one hidden layer, and stops when max.iterations is reached.
#'
#' @param X.mat numeric feature matrix of size [n_observations x n_features]. 
#' @param y.vec numeric label vector of length n_observations.
#' @param max.iterations integer scalar greater than 1.
#' @param step.size numeric non-negative scalar.
#' @param n.hidden.units number of hidden units, greater than or equal to 1.
#' @param is.train logical vector of length n_observations, TRUE if the observation is for training, FALSE for validation
#'
#' @return result.list with named elements:
#' pred.mat, n_observations x max.iterations matrix of predicted values.
#' W.mat final weight matrix (n_features+1 x n.hidden.units).
#' v.vec final weight vector (n.hidden.units+1).
#' predict(testX.mat) a function that takes a test features matrix and returns a vector of predictions.
#' 
#' @export
#'
#' @examples
NNetIteration <- function(X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train){
  
  
  
  
  result.list <- list(pred.mat = ,
                      W.mat = , 
                      v.vec = , 
                      predict = function(testX.mat){
                        
                        prediction.vec <- sigmoid(cbind(1,testX.mat)%*%W.mat)%*%v.vec;
                        
                        return prediction.vec;
                      })
  
  return result.list;
}