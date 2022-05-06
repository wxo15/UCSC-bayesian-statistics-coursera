## Illustrating the use of BIC to estimate the number of components of a Mixture Model
## using the galaxies dataset
rm(list=ls())

### Loading data and setting up global variables
library(MASS)
data(galaxies)
x  = galaxies
n  = length(x)
set.seed(781209)

KKmax = 20
BIC   = rep(0, KKmax-1)

w.sum  = vector("list", KKmax-1)
mu.sum = vector("list", KKmax-1)
sigma.sum = rep(0, KKmax-1)

for(KK in 2:KKmax){
  ### First, compute the "Maximum Likelihood" density estimate 
  ### associated with a location mixture of 6 Gaussian distributions 
  ### using the EM algorithm
  ## Initialize the parameters
  w     = rep(1,KK)/KK
  mu    = rnorm(KK, mean(x), sd(x))
  sigma = sd(x)/KK
  
  epsilon = 0.000001
  s       = 0
  sw      = FALSE
  KL      = -Inf
  KL.out  = NULL
  
  while(!sw){
    ## E step
    v = array(0, dim=c(n,KK))
    for(k in 1:KK){
      v[,k] = log(w[k]) + dnorm(x, mu[k], sigma,log=TRUE)  
    }
    for(i in 1:n){
      v[i,] = exp(v[i,] - max(v[i,]))/sum(exp(v[i,] - max(v[i,])))
    }
    
    ## M step
    # Weights
    w = apply(v,2,mean)
    mu = rep(0, KK)
    for(k in 1:KK){
      for(i in 1:n){
        mu[k]    = mu[k] + v[i,k]*x[i]
      }
      mu[k] = mu[k]/sum(v[,k])
    }
    # Standard deviations
    sigma = 0
    for(i in 1:n){
      for(k in 1:KK){
        sigma = sigma + v[i,k]*(x[i] - mu[k])^2
      }
    }
    sigma = sqrt(sigma/sum(v))
    
    ##Check convergence
    KLn = 0
    for(i in 1:n){
      for(k in 1:KK){
        KLn = KLn + v[i,k]*(log(w[k]) + 
                        dnorm(x[i], mu[k], sigma, log=TRUE))
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
  
  w.sum[[KK-1]]  = w
  mu.sum[[KK-1]] = mu
  sigma.sum[KK-1] = sigma
  
  
  ## Computing BIC
  for(i in 1:n){
    BIC[KK-1] = BIC[KK-1] - 2*log(sum(w*dnorm(x[i], mu, sigma)))
  }
  BIC[KK-1] = BIC[KK-1] + ((KK-1) + 1 + KK)*log(n)  ### KK-1 independent weights, one variance, and KK means
}

## Plot of BIC as a function of K
par(mar=c(4,4,1,1) + 0.1)
plot(seq(2,KKmax), BIC, type="l", xlab="K", ylab="BIC", lwd=2)
abline(v=6, lty=3)

## Computing density estimates for various values of K
density.est = function(xx, w, mu, sigma){
  KK  = length(w)
  nxx = length(xx)
  density.EM = rep(0, nxx)
  for(s in 1:nxx){
    for(k in 1:KK){
      density.EM[s] = density.EM[s] + w[k]*dnorm(xx[s], mu[k], sigma)
    }
  }
  return(density.EM)
}

xx  = seq(5000,37000,length=300)
KK = 8
mdeKK8 = density.est(xx, w.sum[[KK-1]], mu.sum[[KK-1]], sigma.sum[KK-1])
KK = 7
mdeKK7 = density.est(xx, w.sum[[KK-1]], mu.sum[[KK-1]], sigma.sum[KK-1])
KK = 6
mdeKK6 = density.est(xx, w.sum[[KK-1]], mu.sum[[KK-1]], sigma.sum[KK-1])
KK = 5
mdeKK5 = density.est(xx, w.sum[[KK-1]], mu.sum[[KK-1]], sigma.sum[KK-1])
KK = 4
mdeKK4 = density.est(xx, w.sum[[KK-1]], mu.sum[[KK-1]], sigma.sum[KK-1])

## Comparing density estimates for K=4, 5 and 6
par(mar=c(4,4,1,1)+0.1)
plot(xx, mdeKK6, type="n",ylim=c(0,max(c(mdeKK4,mdeKK5,mdeKK6,mdeKK7))), 
     xlab="Velocity", ylab="Density")
lines(xx, mdeKK6, col="black", lty=1, lwd=2)
lines(xx, mdeKK5, col="red", lty=2, lwd=2)
lines(xx, mdeKK4, col="blue", lty=3, lwd=2)
points(x, rep(0,n))
legend(26000, 0.00022, c("K = 6","K = 5","K = 4"), 
       lty=c(1,2,3), col=c("black","red","blue"), bty="n")

## Comparing density estimates for K=6, 7 and 8
par(mar=c(4,4,1,1)+0.1)
plot(xx, mdeKK6, type="n",ylim=c(0,max(c(mdeKK6,mdeKK7,mdeKK8))), 
     xlab="Velocity", ylab="Density")
lines(xx, mdeKK6, col="black", lty=1, lwd=2)
lines(xx, mdeKK7, col="red", lty=2, lwd=2)
lines(xx, mdeKK8, col="blue", lty=3, lwd=2)
points(x, rep(0,n))
legend(26000, 0.00022, c("K = 6","K = 7","K = 8"), 
       lty=c(1,2,3), col=c("black","red","blue"), bty="n")


## What happens with the variance (bandwidth) as K increases
par(mar=c(4,4,1,1) + 0.1)
plot(seq(2,KKmax), sigma.sum, type="l", xlab="K", 
     ylab=expression(hat(sigma)), lwd=2)
abline(v=6, lty=3)

