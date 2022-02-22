
rm(list=ls())
data("warpbreaks")
?warpbreaks
head(warpbreaks)

library("rjags")

mod3_string = " model {
    for( i in 1:length(y)) {
y[i] ~ dnorm(mu[woolGrp[i], tensGrp[i]], prec[woolGrp[i], tensGrp[i]])
}

for (j in 1:max(woolGrp)) {
for (k in 1:max(tensGrp)) {
mu[j,k] ~ dnorm(0.0, 1.0/1.0e6)
prec[j,k] ~ dgamma(1.0/2.0, 1.0/2.0)
}
}

sig = sqrt(1.0 / prec)
} "

str(warpbreaks)

data3_jags = list(y=log(warpbreaks$breaks), woolGrp=as.numeric(warpbreaks$wool), tensGrp=as.numeric(warpbreaks$tension))

params3 = c("mu", "sig")

mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=5e3)
mod3_csim = as.mcmc(do.call(rbind, mod3_sim))

plot(mod3_sim, ask=TRUE)

(dic3 = dic.samples(mod3, n.iter=1e3))

## convergence diagnostics
gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
effectiveSize(mod3_sim)
raftery.diag(mod3_sim)