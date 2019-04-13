library(ElemStatLearn)
library(NeuralNetwork)

data(spam, package = "ElemStatLearn")
data(SAheart, package = "ElemStatLearn")
data(zip.train, package = "ElemStatLearn")
zip.train <- zip.train[zip.train[, 1] %in% c(0, 1),]
data(prostate, package = "ElemStatLearn")
data(ozone, package = "ElemStatLearn")

data.list <- list(
  # spam = list(
  #   features = as.matrix(spam[, 1:57]),
  #   labels = ifelse(spam$spam == "spam", 1, -1),
  #   step.size = 0.01,
  #   is.01 = TRUE
  # ),
  #
    # SAheart = list(
    #   features = as.matrix(SAheart[, c(1:4, 6:9)]),
    #   labels = ifelse(SAheart$chd == 1, 1, -1),
    #   step.size = 0.02,
    # 
    #   is.01 = TRUE
    # ),
    # 
    zip.train = list(
      features = as.matrix(zip.train[,-1]),
      labels = ifelse(zip.train[, 1] == 1, 1, -1),
      step.size = 0.01,

      is.01 = TRUE
  #   ),
  # 
  # 
  #   prostate = list(
  #     features = as.matrix(prostate[, 1:8]),
  #     labels = prostate$lpsa,
  #     step.size = 0.02,
  # 
  #     is.01 = FALSE
  #   ),
  # 
  # ozone = list(
  #   features = as.matrix(ozone[,-1]),
  #   labels = ozone[, 1],
  #   step.size = 0.02,
  #   
  #   is.01 = FALSE
  ))

n.folds <- 5L
n.hidden.units <- 10L
max.iterations <- 30L

for (data.name in names(data.list)) {
  data.set <- data.list[[data.name]]
  
  test.loss.mat <- matrix(0, nrow = n.folds, ncol = 2)
  step.size <- data.set$step.size
  
  #Check data type here:
  
  set.seed(1)
  
  fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))
  
  for (i.fold in (1:n.folds)) {
    train.index <- fold.vec != i.fold
    test.index <- fold.vec == i.fold
    
    X.train <- data.set$features[train.index,]
    y.train <- data.set$labels[train.index]
    X.test <- data.set$features[test.index,]
    y.test <- data.set$labels[test.index]
    
    result.list <- NNetEarlyStoppingCV(
      X.mat = X.train,
      y.vec = y.train,
      max.iterations = max.iterations,
      step.size = step.size,
      n.hidden.units = n.hidden.units
    )
    
    if (data.set$is.01) {
      # binary data
      NNet.predict <-
        ifelse(result.list$predict(X.test) > 0.5, 1,-1)
      NNet.loss <- mean(ifelse(NNet.predict == y.test, 0, 1))
      
      baseline.predict <- mean(y.test)
      
      
    } else{
      # regression data
      NNet.predict <- result.list$predict(X.test)
      NNet.loss <- mean((NNet.predict - y.test) ^ 2)
      
      baseline.predict <- mean(y.test)
      
    }
    
    
    baseline.loss <- mean((baseline.predict - y.test) ^ 2)
    
    test.loss.mat[i.fold,] = c(NNet.loss, baseline.loss)
  }
  
  colnames(test.loss.mat) <- c("Nerual Network", "Baseline")
  
  
  # plot result
  if (!data.set$is.01) {
    barplot(
      test.loss.mat,
      main = c("Square Regression: ", data.name),
      xlab = "mean loss value",
      legend = (rownames(test.loss.mat)),
      beside = TRUE
    )
  } else{
    barplot(
      test.loss.mat,
      main = c("Binary Classification: ", data.name),
      xlab = "mean loss value",
      legend = (rownames(test.loss.mat)),
      beside = TRUE
    )
  }
  
  model.list <- NNetEarlyStoppingCV(
    X.mat = X.train,
    y.vec = y.train,
    max.iterations = max.iterations,
    step.size = step.size,
    n.hidden.units = n.hidden.units
  )
  
  
  
  matplot(
    x = seq(1, 500),
    y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
    xlab = "Iteration",
    ylab = "mean loss value",
    type = "l",
    lty = 1:2,
    pch = 15,
    col = c(17)
  )
  
  
  dot.x <- model.list$selected.step
  dot.y <- model.list$mean.validation.loss.vec[dot.x]
  
  matpoints(x = dot.x,
            y = dot.y,
            col = 2,
            pch = 19)
  legend(
    x = 0,
    y = max(cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec)),
    c("Validation loss", "Train loss"),
    lty = 1:2,
    xjust = 0,
    yjust = 1
  )
}




# X.mat <- data.set$features
# y.vec <- data.set$labels
#

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
