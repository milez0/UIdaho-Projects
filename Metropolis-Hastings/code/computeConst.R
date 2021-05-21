# @description
# Compute c(alpha, sigma) using a grid generated from importance sampling.
# Must first generate a grid using computeConst.grid
# alpha will be rounded, sigma still precise.
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# alpha SCALAR or VECTOR, alpha value to approximate
# sigma SCALAR, sigma parameter value
# grid MATRIX (N by n), grid of values computed by grid function
# 
# @return
# const SCALAR or VECTOR, value of function 
#

computeConst = function(alpha, sigma, step, grid) {
  i = max(round(alpha, -log(step, 10)), step)/step
  mat = as.matrix(exp(-sigma*grid[,i]))
  return(colMeans(mat))
}

# @param
# N SCALAR, number of samples for each choice of alpha
# step SCALAR, increment number for alpha
# 
# @return
# grid MATRIX (n by 10/step) 
#
computeConst.grid = function(N, step) {
  alpha.grid = seq(step, 10, step)
  n = length(alpha.grid)
  grid = matrix(nrow = N, ncol = n)
  for (i in 1:n) {
    alpha = alpha.grid[i]
    x = rdirichlet(N, rep(alpha/4, 4))
    grid[,i] = apply(x^2, 1, sum)
  }
  return(grid)
}

# END
