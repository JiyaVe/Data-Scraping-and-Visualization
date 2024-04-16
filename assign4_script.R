### Paste all your codes for model building
### and cross-validation here

data <- read.csv("assign4_train.csv")
data <- as.matrix(data)

y <- data[,1]  #response
X <- data[,-1] #Covariate matrix

n <- dim(X)[1]

#MLE Ridge regression 

ridge_reg <- function(X,y,lam){
  beta.ridge <- solve(t(X) %*% X + diag(lam, dim(X)[2])) %*% t(X) %*% y
  #sigma2.ridge <- as.numeric((t(y-X %*% beta.ridge) %*% (y-X %*% beta.ridge))/n)
  #sigma2 <- diag(sigma2.ridge,500)
  Y <- X %*% beta.ridge #y_predicted 
  return(Y)
}


#Cross validation

# Vector of lambdas
lam.vec <- 10^(seq(2.3, 2.7, by = 0.01))

# Will store CV error in this
CV.error <- numeric(length = length(lam.vec))

# For each lambda, we will do ridge regression
for (l in 1:length(lam.vec)) {
  track.cv <- 0
  lam <- lam.vec[l]
  for (i in 1:n) {
    # Making training data
    X.train <- X[-i, ] # removing ith row from X
    y.train <- y[-i]   # removing ith element from y
    # Fitting model for training data
    beta.train <- solve(t(X.train) %*% X.train + diag(lam, dim(X)[2])) %*% t(X.train) %*% y.train
    # Test error
    track.cv <- track.cv + (y[i] - X[i, ] %*% beta.train)^2
  }
  CV.error[l] <- track.cv / n
}

# Find the best lambda
chosen.lam <- lam.vec[which.min(CV.error)]

# Save the model
save(list = c("chosen.lam", "ridge_reg"), file = "fit_params.Rdata")
