#### Example of an MCMC algorithm for fitting a mixtures of K p-variate Gaussian components
#### The algorithm is tested using simulated data

## Clear the environment and load required libraries
rm(list=ls())
library(mvtnorm)
library(ellipse)
library(MCMCpack)

## Generate data from a mixture with 3 components
KK      = 3
p       = 2
w.true = c(0.5,0.3,0.2)  # True weights associated with the components
mu.true     = array(0, dim=c(KK,p))
mu.true[1,] = c(0,0)   #True mean for the first component
mu.true[2,] = c(5,5)   #True mean for the second component
mu.true[3,] = c(-3,7)   #True mean for the third component
Sigma.true      = array(0, dim=c(KK,p,p))
Sigma.true[1,,] = matrix(c(1,0,0,1),p,p)   #True variance for the first component
Sigma.true[2,,] = matrix(c(2,0.9,0.9,1),p,p)   #True variance for the second component
Sigma.true[3,,] = matrix(c(1,-0.9,-0.9,4),p,p)   #True variance for the third component
set.seed(63252)    #Keep seed the same so that we can reproduce results
n  = 120
cc.true = sample(1:3, n, replace=T, prob=w.true)
x  = array(0, dim=c(n,p))
for(i in 1:n){
  x[i,] = rmvnorm(1, mu.true[cc.true[i],], Sigma.true[cc.true[i],,])
}

par(mfrow=c(1,1))
plot(x[,1], x[,2], col=cc.true, xlab=expression(x[1]), ylab=expression(x[2]), type="n")
text(x[,1], x[,2], seq(1,n), col=cc.true, cex=0.6)
for(k in 1:KK){
  lines(ellipse(x=Sigma.true[k,,], centre=mu.true[k,], level=0.50), col="grey", lty=2)
  lines(ellipse(x=Sigma.true[k,,], centre=mu.true[k,], level=0.82), col="grey", lty=2)
  lines(ellipse(x=Sigma.true[k,,], centre=mu.true[k,], level=0.95), col="grey", lty=2)
}
title(main="Data + True Components")

## Initialize the parameters
w          = rep(1,KK)/KK  #Assign equal weight to each component to start with
mu         = rmvnorm(KK, apply(x,2,mean), var(x))   #RandomCluster centers randomly spread over the support of the data
Sigma      = array(0, dim=c(KK,p,p))  #Initial variances are assumed to be the same
Sigma[1,,] = var(x)/KK  
Sigma[2,,] = var(x)/KK
Sigma[3,,] = var(x)/KK
cc         = sample(1:KK, n, replace=TRUE, prob=w)

par(mfrow=c(1,1))
plot(x[,1], x[,2], col=cc.true, xlab=expression(x[1]), ylab=expression(x[2]))
for(k in 1:KK){
  lines(ellipse(x=Sigma[k,,], centre=mu[k,], level=0.50), col="grey", lty=2, lwd=2)
  lines(ellipse(x=Sigma[k,,], centre=mu[k,], level=0.82), col="grey", lty=2, lwd=2)
  lines(ellipse(x=Sigma[k,,], centre=mu[k,], level=0.95), col="grey", lty=2, lwd=2)
}
title(main="Initial estimate + Observations")

# Priors
aa = rep(1, KK)
dd = apply(x,2,mean)
DD = 10*var(x)
nu = p
SS = var(x)/3

# Number of iteration of the sampler
rrr = 1000

# Storing the samples
cc.out    = array(0, dim=c(rrr, n))
w.out     = array(0, dim=c(rrr, KK))
mu.out    = array(0, dim=c(rrr, KK, p))
Sigma.out = array(0, dim=c(rrr, KK, p, p))
logpost   = rep(0, rrr)

for(s in 1:rrr){
  # Sample the indicators
  for(i in 1:n){
    v = rep(0,KK)
    for(k in 1:KK){
      v[k] = log(w[k]) + mvtnorm::dmvnorm(x[i,], mu[k,], Sigma[k,,], log=TRUE)  #Compute the log of the weights
    }
    v = exp(v - max(v))/sum(exp(v - max(v)))
    cc[i] = sample(1:KK, 1, replace=TRUE, prob=v)
  }
  
  # Sample the weights
  w = as.vector(rdirichlet(1, aa + tabulate(cc)))
  
  # Sample the means
  DD.st = matrix(0, nrow=p, ncol=p)
  for(k in 1:KK){
    mk    = sum(cc==k)
    xsumk = apply(x[cc==k,], 2, sum)
    DD.st = solve(mk*solve(Sigma[k,,]) + solve(DD))
    dd.st = DD.st%*%(solve(Sigma[k,,])%*%xsumk + solve(DD)%*%dd)
    mu[k,] = as.vector(rmvnorm(1,dd.st,DD.st))
  }
  
  # Sample the variances
  xcensumk = array(0, dim=c(KK,p,p))
  for(i in 1:n){
    xcensumk[cc[i],,] = xcensumk[cc[i],,] + (x[i,] - mu[cc[i],])%*%t(x[i,] - mu[cc[i],])
  }
  for(k in 1:KK){
    Sigma[k,,] = riwish(nu + sum(cc==k), SS + xcensumk[k,,])
  }
  
  # Store samples
  cc.out[s,]      = cc
  w.out[s,]       = w
  mu.out[s,,]     = mu
  Sigma.out[s,,,] = Sigma
  for(i in 1:n){
    logpost[s] = logpost[s] + log(w[cc[i]]) + mvtnorm::dmvnorm(x[i,], mu[cc[i],], Sigma[cc[i],,], log=TRUE)
  }
  logpost[s] = logpost[s] + ddirichlet(w, aa)
  for(k in 1:KK){
    logpost[s] = logpost[s] + mvtnorm::dmvnorm(mu[k,], dd, DD, log=TRUE)
    logpost[s] = logpost[s] + log(diwish(Sigma[k,,], nu, SS))
  }
  
  if(s/250==floor(s/250)){
    print(paste("s = ", s))
  }
  
}

## Plot the logposterior distribution for various samples
par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1)
plot(logpost, type="l", xlab="Iterations", ylab="Log posterior")

## Plot the density estimate for the last iteration of the MCMC
par(mfrow=c(1,1))
par(mar=c(4,4,2,1)+0.1)
plot(x[,1], x[,2], col=cc.true, main=paste("s =",s,"   logpost =", round(logpost[s],4)), xlab=expression(x[1]), ylab=expression(x[2]))
for(k in 1:KK){
  lines(ellipse(x=Sigma[k,,], centre=mu[k,], level=0.50), col="grey", lty=2, lwd=2)
  lines(ellipse(x=Sigma[k,,], centre=mu[k,], level=0.82), col="grey", lty=2, lwd=2)
  lines(ellipse(x=Sigma[k,,], centre=mu[k,], level=0.95), col="grey", lty=2, lwd=2)
}
