
# @description
# 
# This program has 3 main features :
#   1: Generate data from a multivariate normal mixture distribution
#   2: Run an EM algorithm on a data set to identify parameters for a multivariate normal mixture model
#   3: Generate marginal confidence intervals around a multivariate normal mixture model using parametric bootstrap and EM
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# NONE
# 
# @return
# NONE
# 
# -------------------------------------------
# 
# PATHS
# user should change baseDir to location of main.R

baseDir = "D:/Bork/Documents/UIdaho/STAT 565/Assignments/Exam 2/Practice 2"
scriptDir = file.path(baseDir, "code")
outputDir = file.path(baseDir, "data")

# LIBRARIES and FUNCTIONS

library(mvtnorm)
source(file.path(scriptDir, "rmixmvnorm.R")) # Generate n random variables from a multivariate normal mixed model
source(file.path(scriptDir, "dmixmvnorm.R")) # Density function of a multivariate normal mixed model
source(file.path(scriptDir, "EM.R")) # EM function

# SEEDS

set.seed(47)

# INPUT PARAMS
#### Part A ####
# Choose parameters
# EM runtime ~ O(n), largely variable by seed
# Bootstrap runtime ~ O(m*r)

n = 10000 # sample size
m = n
r = 200  # number of parametric resamples

# m = 100 ; r = 1000 ~ 1m
# m = 1000 ; r = 200 ~ 1m
# m = 10000 ; r = 200 ~ 8m

# 1000 resamples would take a couple hours,
# while 10000 would take most of a day.
# I settled on ~1 minute runtime for testing,
# and ~10 minutes for checking consistency.

q = 1 # number of times to run EM to find global max log-likelihood

alpha = .05 # value for bootstrap confidence intervals
epsilon = 10^(-20) # precision of EM (squared)

# true parameters :
mix = c(.2, .2, .3, .3)
mu = list(
  c(0,0),
  c(1,10),
  c(-5,6),
  c(4,-1)
)
sigma = list(
  matrix(c(1, 1,
           1, 2), nrow = 2),
  matrix(c(2, -2,
           -2, 3), nrow = 2),
  matrix(c(3, 0,
           0, 4), nrow = 2),
  matrix(c(4, 4,
           4, 5), nrow = 2)
)

parameters.in = list(n, m, r, alpha, epsilon, mix, mu, sigma)
names(parameters.in) = list("n", "m", "r", "alpha", "epsilon", "mix", "mu", "sigma")


# OUTPUT PARAMS
#### Part B ####
# Generate data
X = rmixmvnorm(n, mix, mu, sigma)

# CODE
#### Part C ####
# Run EM algorithm
# I run it a few times and choose the results with the greatest (log) likelihood
results.final = NULL
ll.old = -Inf
for (i in 1:q) {
  results = EM(X, 4, epsilon)
  mix.em = results$mix
  mu.em = results$mu
  sigma.em = results$sigma
  ll = sum(log(apply(X, 1, dmixmvnorm, mix = mix.em, mu = mu.em, sigma = sigma.em)))
  if (ll > ll.old) {
    results.final = results
    ll.old = ll
  }
  #print(c(i, ll, ll.old, mix.em))
}

results = results.final
mix.em = results$mix
mu.em = results$mu
sigma.em = results$sigma

#### Part D ####
bootstrap = NULL
system.time(for (i in 1:r) {
  X.bs = rmixmvnorm(m, mix.em, mu.em, sigma.em)
  results.bs = EM(X.bs, 4, epsilon, mix.em, mu.em, sigma.em)
  bootstrap = rbind(bootstrap, unlist(results.bs))
})

bootstrap = unique(bootstrap, MARGIN = 2)

confidence.intervals = apply(bootstrap, 2, quantile, probs = c(alpha/2, 1-alpha/2))

# With a little bit of interpretation and comparison to unlist(results), 
# these are the proper parametric bootstrap confidence intervals

# OUTPUT

data.out = list(X, results, confidence.intervals)
names(data.out) = c('X', 'results', 'confidence.intervals')
save(parameters.in, file = file.path(outputDir, "params.dat"))
save(data.out, file = file.path(outputDir, "data.dat"))

# END
