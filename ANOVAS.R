# Bayesian Anovas of plant growth!
# Import Jaggs
library("rjags")

data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)

# Plot the data
boxplot(weight ~ group, data=PlantGrowth)

# Model it
mod_string = " model {
    for (i in 1:length(y)) {
y[i] ~ dnorm(mu[grp[i]], prec)
}

for (j in 1:3) {
mu[j] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*1.0/2.0)
sig = sqrt( 1.0 / prec )
} "

set.seed(84)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains




## Change the model to have different group variances across levels. 

# Model it
mod_string2 = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
}

for (j in 1:3) {
mu[j] ~ dnorm(0.0, 1.0/1.0e6)
prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)

}
sig = sqrt( 1.0 / prec )
} "

str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod2 = jags.model(textConnection(mod_string2), data=data_jags, inits=inits, n.chains=3)
update(mod2, 1e3)

mod_sim2 = coda.samples(model=mod2,
                       variable.names=params,
                       n.iter=5e3)
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2)) # combined chains

# Lets check in on our new model
plot(mod_sim2)

gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
effectiveSize(mod_sim2)

(pm_params = colMeans(mod_csim2))

# Compare the models
summary(mod_csim2)
summary(mod_csim)

dic1=dic.samples(mod, n.iter=1e3)
dic2=dic.samples(mod2, n.iter=1e3)


 = dic1 - dic2

# Use the original model (single variance) to calculate a 95% interval of highest posterior density (HPD) for μ3−μ1 \mu_3 - \mu_1 μ3−μ1. Which of the following is closest to this interval?
meandiff = mod_csim[,3] - mod_csim[,1]
HPDinterval(meandiff, .95)