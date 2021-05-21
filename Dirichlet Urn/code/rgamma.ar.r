# @description
# Simulate random variables from the Gamma distribtuions
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @params
# n SCALAR, number of samples
# shape SCALAR, shape parameter of Gamma distribution
# scale SCALAR, scale parameter of Gamma distribution
#
# @return
# X VECTOR length n, vector of samples

rgamma.ar <- function(n, shape, scale=1) {#random number generator for gamma as dir dist is a generalization of it
  #inputs are n=number of samples, alphas, and scale. scale is default to 1
  s <- shape
  s.int <- floor(s)
  b <- s / s.int
  M <- gamma(s.int) / gamma(s) * b**s.int * (s * exp(-1))**(s - s.int)
  f <- function(y) dgamma(y, shape=s)
  Mg <- function(y) M * dgamma(y, shape=s.int, rate=1 / b)
  acpt <- 0
  total <- 0
  X <- numeric(n)
  while(acpt < n) {
    total <- total + 1
    Y <- sum(-b * log(runif(s.int))) #generating uniform random variable for cut off
    if(runif(1) <= f(Y) / Mg(Y)) {# rejection/acceptance statement with cut offs and uniform random variable creation for dist
      acpt <- acpt + 1
      X[acpt] <- Y
    }
  }
  return(X=scale * X) #return random number from gamma dist.
}

## END
