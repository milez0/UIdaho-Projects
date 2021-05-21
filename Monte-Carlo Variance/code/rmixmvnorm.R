# @description
# Random number generator for data following a multivariate normal mixed distribution
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# n SCALAR, number of data points to simulate
# mix VECTOR length r, mixture probabilities (or proportions)
# mu LIST (length r) of VECTORS (length s), mean vectors
# sigma LIST (length r) of MATRICES (s by s), covariance matrices
# 
# @return
# X MATRIX (n by r), n rows of data generated
#

rmixmvnorm = function(n, mix, mu, sigma) {
  r = length(mix)
  X.sort = NULL
  sort = rmultinom(1, n, mix)
  for (i in 1:r) {
    X.sort = rbind(X.sort, rmvnorm(sort[i], mu[[i]], sigma[[i]]))
  }
  mix.sample = sample(n, n)
  X = X.sort[mix.sample,]
  return(X)
}

# END