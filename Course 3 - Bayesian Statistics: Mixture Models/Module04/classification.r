## Using mixture models for classification in the wine dataset
## Compare linear and quadratic discriminant analysis and a 
##   (semisupervised) location and scale mixture model with K normals
## Comparing only against the EM algorithm

# Semi-supervised, quadratic discriminat analysis 
### Loading data and setting up global variables
library(MASS)
library(mvtnorm)
wine.training = read.table("wine_training.txt", sep=",", header=TRUE)
wine.test = read.table("wine_test.txt", sep=",", header=TRUE)
n = dim(wine.training)[1]  # Size of the training set
m = dim(wine.test)[1]      # Size of the test set
x = rbind(as.matrix(wine.training[,-1]), as.matrix(wine.test[,-1]))   # Create dataset of observations, first n belong to the training set, and the rest belong to the test set
p       = dim(x)[2]              # Number of features
KK      = 3
epsilon = 0.00001

par(mfrow=c(1,1))
par(mar=c(2,2,2,2)+0.1)
colscale = c("black","red","blue")
pairs(wine.training[,-1], col=colscale[wine.training[,1]], pch=wine.training[,1])


# Initialize the parameters of the algorithm
set.seed(63252)
w   = rep(1,KK)/KK  #Assign equal weight to each component to start with
mu  = rmvnorm(KK, apply(x,2,mean), var(x))   #Cluster centers randomly spread over the support of the data
Sigma      = array(0, dim=c(KK,p,p))  #Initial variances are assumed to be the same
Sigma[1,,] = var(x)/KK  
Sigma[2,,] = var(x)/KK
Sigma[3,,] = var(x)/KK

sw     = FALSE
KL     = -Inf
KL.out = NULL
s      = 0

while(!sw){
  ## E step
  v = array(0, dim=c(n+m,KK))
  for(k in 1:KK){  #Compute the log of the weights
    v[1:n,k] = ifelse(wine.training[,1]==k,0,-Inf)  # Training set
    v[(n+1):(n+m),k] = log(w[k]) + mvtnorm::dmvnorm(x[(n+1):(n+m),], mu[k,], Sigma[k,,],log=TRUE)  # Test set
  }
  for(i in 1:(n+m)){
    v[i,] = exp(v[i,] - max(v[i,]))/sum(exp(v[i,] - max(v[i,])))  #Go from logs to actual weights in a numerically stable manner
  }
  
  ## M step
  w = apply(v,2,mean)
  mu = matrix(0, nrow=KK, ncol=p)
  for(k in 1:KK){
    for(i in 1:(n+m)){
      mu[k,]    = mu[k,] + v[i,k]*x[i,]
    }
    mu[k,] = mu[k,]/sum(v[,k])
  }
  Sigma = array(0,dim=c(KK,p,p))
  for(k in 1:KK){
    for(i in 1:(n+m)){
      Sigma[k,,] = Sigma[k,,] + v[i,k]*(x[i,] - mu[k,])%*%t(x[i,] - mu[k,])
    }
    Sigma[k,,] = Sigma[k,,]/sum(v[,k])
  }
  
  ##Check convergence
  KLn = 0
  for(i in 1:(n+m)){
    for(k in 1:KK){
      KLn = KLn + v[i,k]*(log(w[k]) + mvtnorm::dmvnorm(x[i,],mu[k,],Sigma[k,,],log=TRUE))
    }
  }
  if(abs(KLn-KL)/abs(KLn)<epsilon){
    sw=TRUE
  }
  KL = KLn
  KL.out = c(KL.out, KL)
  s = s + 1
  print(paste(s, KLn))
}

## Predicted labels
apply(v, 1, which.max)[(n+1):(n+m)]
## True labels
wine.test[,1]
## Comparison
apply(v, 1, which.max)[(n+1):(n+m)] == wine.test[,1]
sum(!(apply(v, 1, which.max)[(n+1):(n+m)] == wine.test[,1])) # One error

# Using the qda and lda functions in R
# qda
modqda = qda(grouping=wine.training[,1], x=wine.training[,-1], method="mle")
ccpredqda = predict(modqda,newdata=wine.test[,-1])
sum(!(ccpredqda$class == wine.test[,1])) # One error

# lda
modlda = lda(grouping=wine.training[,1], x=wine.training[,-1], method="mle")
ccpredlda = predict(modlda,newdata=wine.test[,-1])
sum(!(ccpredlda$class == wine.test[,1])) # No errors!!!
