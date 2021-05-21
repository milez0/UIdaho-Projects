# @description
# Run EM algorithm on data assuming a multivariate normal mixed model
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# X MATRIX n by r, data
# epsilon SCALAR, stopping precision (squared L2 norm)
# mix VECTOR length r, mixture probabilities (or proportions)
# mu LIST (length r) of VECTORS (length s), mean vectors
# sigma LIST (length r) of MATRICES (s by s), covariance matrices
# 
# @return
# results LIST (length 3) of
#   mix VECTOR length r, mixture probabilities (or proportions)
#   mu LIST (length r) of VECTORS (length s), mean vectors
#   sigma LIST (length r) of MATRICES (s by s), covariance matrices
#

EM = function(X, r, epsilon, mix = NULL, mu = NULL, sigma = NULL) {
  n = nrow(X)
  s = ncol(X)
  diff = Inf
  
  if (is.null(mix)) {
    mix = runif(r)
  }
  mix = mix/sum(mix)
  if (is.null(mu)) {
    mu.sample = sample(n, r)
    mu = lapply(apply(X[mu.sample,], 1, list), unlist)
  }
  if (is.null(sigma)) {
    sigma = rep(list(cov(X)), r)
  }
  
  while (diff > epsilon) {
    mix.old = mix
    mu.old = mu
    sigma.old = sigma
    old.vec = c(mix.old, sapply(mu.old, c), sapply(sigma.old, c))
    
    # Calculate PI(x) for all (j,k) 
    mix.ex = NULL
    for (j in 1:r) {
      mix.ex = cbind(mix.ex, mix[[j]]*dmvnorm(X, mu[[j]], sigma[[j]]))
    }
    mix.ex = mix.ex/rowSums(mix.ex)
    
    # Update PI
    mix = colSums(mix.ex)
    mix = mix/sum(mix)
    
    mix.sums = colSums(mix.ex)
    
    for(j in 1:r) {
      # Update MU
      mu[[j]] = colSums(X*mix.ex[,j])/mix.sums[j]
      
      # Update SIGMA
      sigma.sum = diag(c(0,0))
      for (k in 1:n) {
        sigma.sum.diff = X[k,]-mu[[j]]
        sigma.sum = sigma.sum + mix.ex[k,j]*(sigma.sum.diff%*%t(sigma.sum.diff))
      }
      sigma[[j]] = sigma.sum/mix.sums[j]
      sigma[[j]]
    }
    new.vec = c(mix, sapply(mu, c), sapply(sigma, c))
    diff = sum((new.vec - old.vec)^2) # L2 norm squared. No real reason to sqrt, just square epsilon
  }
  results = list(mix, mu, sigma)
  names(results) = c("mix", "mu", "sigma")
  return(results)
}

# END
