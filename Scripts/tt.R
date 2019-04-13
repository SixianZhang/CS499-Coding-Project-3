library(ElemStatLearn)
library(NeuralNetwork)

data(spam, package = "ElemStatLearn")
data(prostate, package = "ElemStatLearn")
data(ozone, package = "ElemStatLearn")
# X.mat <- as.matrix(spam[,1:57])
# y.vec <- as.vector(spam$spam)
# y.vec <- ifelse(y.vec == "spam", 1, -1)

# X.mat <- as.matrix(prostate[,1:8])
# y.vec <- as.vector(prostate$lpsa)

X.mat <- as.matrix(ozone[, -1])
y.vec <- as.vector(ozone[, 1])

if (all(y.vec %in% c(-1,1)))
{
  is.binary <- 1
}else
{
  is.binary <- 0  
}

result.list <- NNetEarlyStoppingCV(X.mat, y.vec, max.iterations = 500L, step.size = 0.01, 
                                   n.hidden.units = 200L, n.folds = 6L)

matplot(1:500,as.matrix(result.list$mean.validation.loss.vec), type = 'l', lty = 1:2,
        pch = 15,
        col = c(17))





