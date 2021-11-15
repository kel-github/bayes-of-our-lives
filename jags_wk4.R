rm(list=ls())

# 1. Specify the model
library(rjags)

mod_string = " model {
  for (i in 1:n) {
    y[i] ~ dnorm(mu, 1.0/sig2) 
  }
  mu ~ dt(0.0, 1.0/1.0, 1)
  sig2 = 0.5
} "

# 2. set up the model
set.seed(42)
y <- c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
n = length(y)

data_jags = list(y=y, n=n)
params = c("mu")

inits <- function(){
  inits <- list("mu"= 0.0)
}

mod = jags.model(textConnection(mod_string), data = data_jags, init=inits)

# 3. Run the MCMC sampler
update(mod, 500)
mod_sim = coda.samples(model = mod, variable.names = params, n.iter = 1000)

# 4. Post processing
library(coda)

plot(mod_sim)
summary(mod_sim)
