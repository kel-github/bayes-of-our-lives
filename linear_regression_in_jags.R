## set up data and model
library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

summary(lm(education~income+young+urban, data= Anscombe))

modlm = lm(education~income+young+urban, data= Anscombe)

library("rjags")

mod_string = " model {
for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
}

b0 ~ dnorm(0.0, 1.0/1.0e6)
for (i in 1:3) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(42)
data_jags = list(education = Anscombe$education,
                 income = Anscombe$income,
                 young = Anscombe$young,
                 urban = Anscombe$urban)

params = c("b0", "b", "sig")

init = function(){
  inits = list("b" = rnorm(3, 0, 100), "prec"=rgamma(1,1.0,1.0), 
               "b0" = rnorm(1, 0, 100))
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

# Assess dic
dic.samples(mod, n.iter=100000)


# Compare adjustments to model
# 1 - drop urban term

mod_string = " model {
for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
}

b0 ~ dnorm(0.0, 1.0/1.0e6)
for (i in 1:2) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(42)
data_jags = list(education = Anscombe$education,
                 income = Anscombe$income,
                 young = Anscombe$young)

params = c("b0", "b", "sig")

init = function(){
  inits = list("b" = rnorm(2, 0, 100), "prec"=rgamma(1,1.0,1.0), 
               "b0" = rnorm(1, 0, 100))
}

mod_urbandrop = jags.model(textConnection(mod_string), data=data_jags,
                 inits = init, n.chains = 4)
update(mod_urbandrop , 1000)


# Step 2 - add interaction

mod_string = " model {
for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
}

b0 ~ dnorm(0.0, 1.0/1.0e6)
for (i in 1:3) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

data_jags = list(education = Anscombe$education,
                 income = Anscombe$income,
                 young = Anscombe$young)

params = c("b0", "b", "sig")

init = function(){
  inits = list("b" = rnorm(3, 0, 100), "prec"=rgamma(1,1.0,1.0), 
               "b0" = rnorm(1, 0, 100))
}

mod_interact = jags.model(textConnection(mod_string), data=data_jags,
                 inits = init, n.chains = 4)

update(mod_interact, 1000)

# Measure the dics

dic.samples(mod_urbandrop, n.iter=100000)
dic.samples(mod_interact, n.iter=100000)
dic.samples(mod, n.iter=100000)


# probabilities of positive things
sum(mod_csim[,"b[1]"]>0.0)/length(mod_csim[,"b[1]"])
