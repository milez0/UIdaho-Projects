# @description
# Generalized polyurn function to support variable length of alpha vector and noninteger alpha values.
# Soution for part b
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @params
# alpha VECTOR length k, initial weighted relative probabilities
# n SCALAR, size of sample
# 
# @return
# results VECTOR length k, cumulative sample, respective to alpha parameter

# DEPENDENCIES
# NONE

polyurn2=function (alpha, n){# function doesn't change much other than alpha input needs to be in vector form prior
  x=myrdirichlet(n,alpha)
  d=length(alpha)
  draw_results=NULL
  for (i in 1:n){
    draw_results=rbind(draw_results, myrmultinom(1, prob= x[i,])) #(size 1 for 1 ball per draw)
  }
  results = colSums(draw_results)
  return(results)
}

## END
