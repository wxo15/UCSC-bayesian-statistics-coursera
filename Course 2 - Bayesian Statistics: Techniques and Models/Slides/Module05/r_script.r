dat = read.csv("/home/wxooi15/Desktop/Bayesian Statistics/Course 2 - Bayesian Statistics: Techniques and Models/Slides/Module05/Car_sales.csv", header=TRUE)

# Take interesting column
dat=dat[c("Manufacturer","Sales_in_thousands","Engine_size","Vehicle_type", "Horsepower","Wheelbase","Width","Length","Curb_weight","Fuel_capacity", "Fuel_efficiency","Power_perf_factor")]

# Remove NA
dat = na.omit(dat)

# Remove Outlier
dat <- dat[dat$Sales_in_thousands < 300,]

# Check data, and approximate curve
x=seq(from=0, to=300, by=0.1)
hist(dat$Sales_in_thousands, breaks=50, freq=FALSE)
# curve(0.02*exp(-x/50),from=0, to=300, add=TRUE)
lines(x=x, y=dgamma(x, shape=1.1, rate=1.1/50))

# pair plot
pairs(dat)

# Power_perf_factor correlated with horsepower. remove
dat=dat[c("Manufacturer","Sales_in_thousands","Engine_size","Vehicle_type", "Horsepower","Wheelbase","Width","Length","Curb_weight","Fuel_capacity", "Fuel_efficiency")]

mod_lm=lm(formula = Sales_in_thousands ~ Manufacturer + Engine_size + 
    Horsepower + Wheelbase + Width + Length + Curb_weight + Fuel_capacity + 
    Fuel_efficiency, data=dat3)
    
# Take |t| > 1
# Ford, Honda, Jeep, Nissan, Porsche, Subaru, Toyota, Engine_size, Horsepower, Curb_weight, Fuel_capacity
#dat$isFord=as.numeric(dat$Manufacturer=="Ford")
#dat$isHonda=as.numeric(dat$Manufacturer=="Honda")
#dat$isJeep=as.numeric(dat$Manufacturer=="Jeep")
#dat$isNissan=as.numeric(dat$Manufacturer=="Nissan")
#dat$isPorsche=as.numeric(dat$Manufacturer=="Porsche")
#dat$isSubaru=as.numeric(dat$Manufacturer=="Subaru")
#dat$isToyota=as.numeric(dat$Manufacturer=="Toyota")
#dat=dat[c("Sales_in_thousands","isFord", "isHonda", "isJeep", "isNissan", "isPorsche", "isSubaru", "isToyota", "Engine_size","Horsepower","Curb_weight","Fuel_capacity")]
dat=dat[c("Sales_in_thousands", "Manufacturer", "Engine_size", "Horsepower", "Curb_weight", "Fuel_capacity")]


library("rjags")
mod_string = " model {
  for (i in 1:length(Sales_in_thousands)) {
    Sales_in_thousands[i] ~ dnorm(mu[i], prec[Manufacturer[i]])
    mu[i] = a[Manufacturer[i]] + b[1]*Engine_size[i] + b[2]*Horsepower[i] + b[3]*Curb_weight[i] + b[4]*Fuel_capacity[i] + b[5]*Wheelbase[i]
  }
  
  for (j in 1:max(Manufacturer)) {
    a[j] ~ dnorm(a0, prec_a)
    prec[j] ~ dgamma(prec0, prec_b)
    sig[j] = sqrt( 1.0 / prec[j] )
  }
  
  a0 ~ dnorm(0.0, 1.0/1.0e6)
  prec0 ~ dgamma(1/2.0, 1*10.0/2.0)
  prec_a ~ dgamma(1/2.0, 1*10.0/2.0)
  prec_b ~ dgamma(1/2.0, 1*10.0/2.0)
  tau_a = sqrt( 1.0 / prec_a )
  tau_b = sqrt( 1.0 / prec_b )
  
  for (j in 1:5) {
    b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  
} "

set.seed(116)
data_jags = as.list(dat)

params = c("a0", "a", "prec0", "sig", "b", "tau_a", "tau_b")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3) # burn-in

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e4)

mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combine multiple chains

## convergence diagnostics
plot(mod_sim, ask=TRUE)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

dic = dic.samples(mod, n.iter=1e3)



dat2 = dat
dat2$newManufacturer <- "Other"
dat2$newManufacturer[dat2$Manufacturer=="Ford"] <- "Ford"
dat2$newManufacturer[dat2$Manufacturer=="Honda"] <- "Honda"
dat2$newManufacturer[dat2$Manufacturer=="Jeep"] <- "Jeep"
dat2$newManufacturer[dat2$Manufacturer=="Nissan"] <- "Nissan"
dat2$newManufacturer[dat2$Manufacturer=="Porsche"] <- "Porsche"
dat2$newManufacturer[dat2$Manufacturer=="Subaru"] <- "Subaru"
dat2$newManufacturer[dat2$Manufacturer=="Toyota"] <- "Toyota"
dat2$newManufacturer <- factor(dat2$newManufacturer)
dat2$Manufacturer <- dat2$newManufacturer
set.seed(116)
data_jags2 = as.list(dat2)

params = c("a0", "a", "sig", "b")

mod2 = jags.model(textConnection(mod_string), data=data_jags2, n.chains=3)
update(mod2, 1e3) # burn-in

mod2_sim = coda.samples(model=mod2,
                       variable.names=params,
                       n.iter=5e4)

mod2_csim = as.mcmc(do.call(rbind, mod2_sim)) # combine multiple chains

## convergence diagnostics
plot(mod2_sim, ask=TRUE)

gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)
autocorr.plot(mod2_sim)
effectiveSize(mod2_sim)

dic2= dic.samples(mod2, n.iter=1e3)

# Extract the parameters's
pm2_a_params = colMeans(mod2_csim)[1:8]
pm2_b_params = colMeans(mod2_csim)[10:13]
pm2_sig_params = colMeans(mod2_csim)[14:21]

dat2$mean = pm2_a_params[dat2$Manufacturer] + pm2_b_params[1]*dat2$Engine_size + pm2_b_params[2]*dat2$Horsepower + pm2_b_params[3]*dat2$Curb_weight + pm2_b_params[4]*dat2$Fuel_capacity

dat2$sig = pm2_sig_params[dat2$Manufacturer]
plot(dat2$mean, dat2$Sales_in_thousands)
plot(dat2$Sales_in_thousands - dat2$mean)




dat3 = dat
dat3$newManufacturer <- "Other"
dat3$newManufacturer[dat3$Manufacturer=="Ford"] <- "Ford"
dat3$newManufacturer[dat3$Manufacturer=="Honda"] <- "Honda"
dat3$newManufacturer[dat3$Manufacturer=="Jeep"] <- "Jeep"
dat3$newManufacturer[dat3$Manufacturer=="Porsche"] <- "Porsche"
dat3$newManufacturer[dat3$Manufacturer=="Subaru"] <- "Subaru"
dat3$newManufacturer[dat3$Manufacturer=="Chevrolet" | dat3$Manufacturer=="Pontiac" |dat3$Manufacturer=="Toyota"] <- "CPT"
dat3$newManufacturer[dat3$Manufacturer=="Dodge"] <- "Dodge"
dat3$newManufacturer[dat3$Manufacturer=="Buick" | dat3$Manufacturer=="Nissan" |dat3$Manufacturer=="Volkswagen" |dat3$Manufacturer=="Mercury"] <- "BMNV"

# Chevrolet, Pontiac
dat3$newManufacturer <- factor(dat3$newManufacturer)
dat3$Manufacturer <- dat3$newManufacturer
dat3=dat3[c("Sales_in_thousands", "Manufacturer", "Engine_size", "Horsepower","Wheelbase", "Width", "Length", "Fuel_capacity")]

mod3_string = " model {
  for (i in 1:length(Sales_in_thousands)) {
    alpha[i] = mu[i]^2/sig[Manufacturer[i]]
    beta[i] = mu[i]/sig[Manufacturer[i]]
    Sales_in_thousands[i] ~ dgamma(alpha[i], beta[i])
    mu[i] = a[Manufacturer[i]] + b[1]*Engine_size[i] + b[2]*Horsepower[i] + b[3]*Wheelbase[i] + b[4]*Width[i] + b[5]*Length[i] + b[6]*Fuel_capacity[i]
  }
  
  for (j in 1:max(Manufacturer)) {
    a[j] ~ dnorm(a0, prec_a)
    prec[j] ~ dgamma(prec0, prec_b)
    sig[j] = sqrt( 1.0 / prec[j] )
  }
  
  a0 ~ dnorm(0.0, 1.0/1.0e6)
  prec0 ~ dgamma(1/2.0, 1*10.0/2.0)
  prec_a ~ dgamma(1/2.0, 1*10.0/2.0)
  prec_b ~ dgamma(1/2.0, 1*10.0/2.0)
  tau_a = sqrt( 1.0 / prec_a )
  tau_b = sqrt( 1.0 / prec_b )
  
  for (j in 1:6) {
    b[j] ~ dnorm(10.0, 1.0/1.0e6) # Start with a positive number to avoid mu < 0 which would break the sales gamma model
  }
  
} "

set.seed(116)
data_jags3 = as.list(dat3)

params = c("a0", "a", "prec0", "sig", "b", "tau_a", "tau_b")

mod3 = jags.model(textConnection(mod3_string), data=data_jags3, n.chains=3)
update(mod3, 1e4) # burn-in

mod3_sim = coda.samples(model=mod3,
                       variable.names=params,
                       n.iter=5e5)

mod3_csim = as.mcmc(do.call(rbind, mod3_sim)) # combine multiple chains

## convergence diagnostics
plot(mod3_sim, ask=TRUE)

gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
autocorr.plot(mod3_sim)
effectiveSize(mod3_sim)

dic3 = dic.samples(mod3, n.iter=1e3)

# Extract the parameters's
pm3_a_params = colMeans(mod3_csim)[1:9]
pm3_b_params = colMeans(mod3_csim)[11:16]
pm3_sig_params = colMeans(mod3_csim)[18:26]

dat3$mean = pm3_a_params[dat3$Manufacturer] + pm3_b_params[1]*dat3$Engine_size + pm3_b_params[2]*dat3$Horsepower + pm3_b_params[3]*dat3$Wheelbase + pm3_b_params[4]*dat3$Width + pm3_b_params[5]*dat3$Length + pm3_b_params[6]*dat3$Fuel_capacity


dat3$sig = pm3_sig_params[dat3$Manufacturer]

plot(dat3$mean, dat3$Sales_in_thousands)

x=seq(from=0, to=300, by=0.1)
hist(dat$Sales_in_thousands[dat$Manufacturer=="Ford"], breaks=50, freq=FALSE)
lines(x=x, y=dgamma(x, shape=2.33, rate=0.018))
hist(dat$Sales_in_thousands[dat$Manufacturer=="Dodge"], breaks=50, freq=FALSE)
lines(x=x, y=dgamma(x, shape=82^2/8429, rate=82/8429))

pairs(dat3[dat3$Manufacturer == "Dodge"])

