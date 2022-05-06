#### Example of an MCMC algorithm for fitting a location mixture of 2 Gaussian components
#### The algorithm is tested using simulated data

## Clear the environment and load required libraries
rm(list=ls())
library(MCMCpack)
set.seed(81196)  # So that results are reproducible

## Generate data from a mixture with 2 components
KK         = 2
w.true     = 0.6  # True weights associated with the components
mu.true    = rep(0, KK)
mu.true[1] = 0   # True mean for the first component
mu.true[2] = 5   # True mean for the second component
sigma.true = 1   # True standard deviation of all components
n          = 120         # Number of observations to be generated
cc.true = sample(1:KK, n, replace=T, prob=c(w.true,1-w.true))
x  = rep(0, n)
for(i in 1:n){
  x[i] = rnorm(1, mu.true[cc.true[i]], sigma.true)
}

# Plot the true density
par(mfrow=c(1,1))
xx.true = seq(-8,11,length=200)
yy.true = w.true*dnorm(xx.true, mu.true[1], sigma.true) + 
  (1-w.true)*dnorm(xx.true, mu.true[2], sigma.true) 
plot(xx.true, yy.true, type="l", xlab="x", ylab="True density", lwd=2)
points(x, rep(0,n), col=cc.true)

## Initialize the parameters
w     = 1/2                         #Assign equal weight to each component to start with
mu    = rnorm(KK, mean(x), sd(x))   #Random cluster centers randomly spread over the support of the data
sigma = sd(x)                       #Initial standard deviation

# Plot the initial guess for the density
xx = seq(-8,11,length=200)
yy = w*dnorm(xx, mu[1], sigma) + (1-w)*dnorm(xx, mu[2], sigma)
plot(xx, yy, type="l", ylim=c(0, max(yy)), xlab="x", 
     ylab="Initial density", lwd=2)
points(x, rep(0,n), col=cc.true)

## The actual MCMC algorithm starts here
# Priors
aa  = rep(1,KK)  # Uniform prior on w
eta = 0          # Mean 0 for the prior on mu_k
tau = 5          # Standard deviation 5 on the prior for mu_l
dd  = 2
qq  = 1

# Number of iterations of the sampler
rrr   = 6000
burn  = 1000


# Storing the samples
cc.out    = array(0, dim=c(rrr, n))
w.out     = rep(0, rrr)
mu.out    = array(0, dim=c(rrr, KK))
sigma.out = rep(0, rrr)
logpost   = rep(0, rrr)

# MCMC iterations
for(s in 1:rrr){
  # Sample the indicators
  cc = rep(0,n)
  for(i in 1:n){
    v = rep(0,KK)
    v[1] = log(w) + dnorm(x[i], mu[1], sigma, log=TRUE)  #Compute the log of the weights
    v[2] = log(1-w) + dnorm(x[i], mu[2], sigma, log=TRUE)  #Compute the log of the weights
    v = exp(v - max(v))/sum(exp(v - max(v)))
    cc[i] = sample(1:KK, 1, replace=TRUE, prob=v)
  }
  
  # Sample the weights
  w = rbeta(1, aa[1] + sum(cc==1), aa[2] + sum(cc==2))

  # Sample the means
  for(k in 1:KK){
    nk    = sum(cc==k)
    xsumk = sum(x[cc==k])
    tau2.hat = 1/(nk/sigma^2 + 1/tau^2)
    mu.hat  = tau2.hat*(xsumk/sigma^2 + eta/tau^2)
    mu[k]   = rnorm(1, mu.hat, sqrt(tau2.hat))
  }

  # Sample the variances
  dd.star = dd + n/2
  qq.star = qq + sum((x - mu[cc])^2)/2
  sigma = sqrt(rinvgamma(1, dd.star, qq.star))

  # Store samples
  cc.out[s,]   = cc
  w.out[s]     = w
  mu.out[s,]   = mu
  sigma.out[s] = sigma
  for(i in 1:n){
    if(cc[i]==1){
      logpost[s] = logpost[s] + log(w) + dnorm(x[i], mu[1], sigma, log=TRUE)
    }else{
      logpost[s] = logpost[s] + log(1-w) + dnorm(x[i], mu[2], sigma, log=TRUE)
    }
  }
  logpost[s] = logpost[s] + dbeta(w, aa[1], aa[2],log = T)
  for(k in 1:KK){
    logpost[s] = logpost[s] + dnorm(mu[k], eta, tau, log = T)
  }
  logpost[s] = logpost[s] + log(dinvgamma(sigma^2, dd, 1/qq))
  if(s/500==floor(s/500)){
    print(paste("s =",s))
  }
}

## Plot the logposterior distribution for various samples
par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1)
plot(logpost, type="l", xlab="Iterations", ylab="Log posterior")

xx = seq(-8,11,length=200)
density.posterior = array(0, dim=c(rrr-burn,length(xx)))
for(s in 1:(rrr-burn)){
  density.posterior[s,] = density.posterior[s,] + w.out[s+burn]*dnorm(xx,mu.out[s+burn,1],sigma.out[s+burn]) +
                                                  (1-w.out[s+burn])*dnorm(xx,mu.out[s+burn,2],sigma.out[s+burn])
}
density.posterior.m = apply(density.posterior , 2, mean)
density.posterior.lq = apply(density.posterior, 2, quantile, 0.025)
density.posterior.uq = apply(density.posterior, 2, quantile, 0.975)
par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1)
plot(xx, density.posterior.m, type="n",ylim=c(0,max(density.posterior.uq)), xlab="x", ylab="Density")
polygon(c(xx,rev(xx)), c(density.posterior.lq, rev(density.posterior.uq)), col="grey", border="grey")
lines(xx, density.posterior.m, lwd=2)
points(x, rep(0,n), col=cc.true)
