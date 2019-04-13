library(ElemStatLearn)
library(NeuralNetwork)

data(spam, package = "ElemStatLearn")
data(SAheart, package = "ElemStatLearn")
data(zip.train, package = "ElemStatLearn")
zip.train <- zip.train[zip.train[, 1] %in% c(0, 1), ]
data(prostate, package = "ElemStatLearn")
data(ozone, package = "ElemStatLearn")

data.list <- list(
  # spam = list(
  #   features = as.matrix(spam[, 1:57]),
  #   labels = ifelse(spam$spam == "spam", 1, -1),
  # 
  #   is.01 = TRUE
  # ),
  # 
  # SAheart = list(
  #   features = as.matrix(SAheart[, c(1:4, 6:9)]),
  #   labels = ifelse(SAheart$chd == 1, 1, -1),
  # 
  #   is.01 = TRUE
  # ),
  # 
  # zip.train = list(
  #   features = as.matrix(zip.train[,-1]),
  #   labels = ifelse(zip.train[, 1] == 1, 1, -1),
  # 
  #   is.01 = TRUE
  # ),
  # 
  
  prostate = list(
    features = as.matrix(prostate[, 1:8]),
    labels = prostate$lpsa,
    is.01 = FALSE
  ),
  
  ozone = list(
    features = as.matrix(ozone[, -1]),
    labels = ozone[, 1],
    is.01 = FALSE
  )
)

n.folds <- 2L

for (data.name in names(data.list)) {
  data.set <- data.list[[data.name]]
  test.loss.mat <- matrix(0, nrow = 4, ncol = 3)
  
  #Check data type here:
  
  set.seed(1)
  
  fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))
  
  for (i.fold in (1:n.folds)) {
    train.index <- fold.vec != i.fold
    test.index <- fold.vec == i.fold
    
    X.train <- data.set$features[train.index, ]
    y.train <- data.set$labels[train.index]
    X.test <- data.set$features[test.index, ]
    y.test <- data.set$labels[test.index]
    
    result.list <- NNetEarlyStoppingCV(X.mat = X.train,
                                       y.vec = y.train,
                                       max.iterations = 500L,
                                       step.size = 0.02,
                                       n.hidden.units = 100L)
    
    if (data.set$is.01) {
      # binary data
     NNet.predict <- result.list$pred.mat
     
     baseline.predict <- mean(y.test)
     
     
    } else{
      # regression data
      NNet.predict <- result.list$pred.mat
      
      baseline.predict <- mean(y.test)
      
    }
    
    
    NNet.loss <- mean((NNet.predict - y.test) ^ 2)
    baseline.loss <- mean((baseline.predict - y.test) ^ 2)
    
    test.loss.mat[i.fold, ] = c(NNet.loss, baseline.loss)
  }
}




# X.mat <- data.set$features
# y.vec <- data.set$labels
# 
# matpoints(x = dot.x,
#           y = dot.y,
#           col = 2,
#           pch = 19)
# legend(
#   x = 0,
#   y = max(
#     cbind(
#       model.list$mean.validation.loss.vec,
#       model.list$mean.train.loss.vec
#     )
#   ),
#   c("Validation loss", "Train loss"),
#   lty = 1:2,
#   xjust = 0,
#   yjust = 1
# )
