# Questions 5 & 6 from https://www.coursera.org/learn/mcmc-bayesian-statistics/exam/BMZrs/lesson-3/attempt

m <- 100001
theta <- rbeta(m, 5, 3)
mean(theta/(1-theta)) # odds of succeeding
se <- (sd(theta/(1-theta))/sqrt(m))*2 # confidence 


odds <- theta/(1-theta)
i <- odds > 1
mean(i)

# 7 - get 0.3 quantile of standard normal
m <- 1000007
theta <- rnorm(m, mean = 0, sd = 1)
quantile(theta, probs = 0.3)
qnorm(0.3, mean = 0, sd = 1)

sqrt(5.2/5000)
