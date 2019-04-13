library(ElemStatLearn)
library(NeuralNetwork)

data(spam, package = "ElemStatLearn")
X.mat <- as.matrix(spam[,1:57])
y.vec <- as.vector(spam$spam)
y.vec <- ifelse(y.vec == "spam", 1, -1)

if (all(y.vec %in% c(-1,1)))
{
  is.binary <- 1
}else
{
  is.binary <- 0  
}

result.list <- NNetEarlyStoppingCV(X.mat, y.vec, max.iterations = 500L, step.size = 0.02, 
                                   n.hidden.units = 100L, n.folds = 4L)






