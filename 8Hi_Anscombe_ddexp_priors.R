# Week 4 8H
# Common models and multiple factor ANOVA
# -------------------------------------------------------
library("car")
data("Anscombe")
head(Anscombe)
?Anscombe

Xc = scale(Anscombe, center=TRUE, scale=TRUE)
str(Xc)

data_jags = as.list(data.frame(Xc))

library("rjags")

mod_string = " model {
for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
}

for (i in 1:3) {
b[i] ~ ddexp(0.0, 1.0)
}

prec ~ dgamma(1.0/2.0, 1.0/2.0)
## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(42)
data_jags = list(education = Xc[,"education"],
                 income = Xc[,"income"],
                 young = Xc[,"young"],
                 urban = Xc[,"urban"])

params = c("b", "sig")

init = function(){
  inits = list("b" = rnorm(3, 0, 100), "prec"=1)
}

mod = jags.model(textConnection(mod_string), data=data_jags,
                 inits = init, n.chains = 4)

update(mod, 1000)

mod_sim = coda.samples(model=mod, variable.names = params, 
                       n.iter=5e3)

mod_csim = do.call(rbind, mod_sim)

# model checking
plot(mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)


# Modelling residuals
plot(modlm)



