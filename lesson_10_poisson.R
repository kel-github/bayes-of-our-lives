library("COUNT")
data("badhealth")
?badhealth
head(badhealth)

# model shiz
library("rjags")

mod_string = " model {
    for (i in 1:length(numvisit)) {
numvisit[i] ~ dpois(lam[i])
log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
}

int ~ dnorm(0.0, 1.0/1e6)
b_badh ~ dnorm(0.0, 1.0/1e4)
b_age ~ dnorm(0.0, 1.0/1e4)
b_intx ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)

#######################################################
# model with only main effects
me_mod_string = " model {
    for (i in 1:length(numvisit)) {
numvisit[i] ~ dpois(lam[i])
log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
}

int ~ dnorm(0.0, 1.0/1e6)
b_badh ~ dnorm(0.0, 1.0/1e4)
b_age ~ dnorm(0.0, 1.0/1e4)
} "

params = c("int", "b_badh", "b_age")
ME_mod = jags.model(textConnection(me_mod_string), data=data_jags, n.chains=3)
update(ME_mod, 1e3)

ME_mod_sim = coda.samples(model=ME_mod,
                          variable.names=params,
                          n.iter=5e3)
ME_mod_csim = as.mcmc(do.call(rbind, ME_mod_sim))

## convergence diagnostics
plot(ME_mod_sim)

gelman.diag(ME_mod_sim)
autocorr.diag(ME_mod_sim)
autocorr.plot(ME_mod_sim)
effectiveSize(ME_mod_sim)

## compute DIC
ME_dic = dic.samples(ME_mod, n.iter=1e3)


## Poisson model
prob=ppois(22, 30)

###############################################
# Callers data

dat = read.csv(file="callers.csv", header=TRUE)
sum(is.na(dat))

# model with only main effects
call_mod_string = " model {
for (i in 1:length(calls)) {
		calls[i] ~ dpois( days_active[i] * lam[i] )
log(lam[i]) = b0 + b1*age[i] + b2*isgroup2[i]
}


b0 ~ dnorm(0.0, 100)
b1 ~ dnorm(0.0, 100)
b2 ~ dnorm(0.0, 100)
} "

params = c("b0", "b1", "b2")
call_mod = jags.model(textConnection(call_mod_string), data=dat, n.chains=3)
update(call_mod, 1e3)

call_mod_sim = coda.samples(model=call_mod,
                          variable.names=params,
                          n.iter=5e3)
call_mod_csim = as.mcmc(do.call(rbind, call_mod_sim))

sum(call_mod_csim[,'b2']>0)/ length(call_mod_csim[,'b2'])

## convergence diagnostics
plot(call_mod_sim)

gelman.diag(call_mod_sim)
autocorr.diag(call_mod_sim)
autocorr.plot(call_mod_sim)
effectiveSize(call_mod_sim)

## compute DIC
call_dic = dic.samples(call_mod, n.iter=1e3)