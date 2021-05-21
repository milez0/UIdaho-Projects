# @description
# Density function for multivariate normal mixed distribution
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# x VECTOR length s, data point
# mix VECTOR length r, mixture probabilities (or proportions)
# mu LIST (length r) of VECTORS (length s), mean vectors
# sigma LIST (length r) of MATRICES (s by s), covariance matrices
# 
# @return
# p SCALAR, density function value
#

dmixmvnorm = function(x, mix, mu, sigma) {
  mix = mix/sum(mix)
  p = sum(mix*mapply(dmvnorm, mu, sigma, MoreArgs = list(x = x)))
  return(p)
}

# END
