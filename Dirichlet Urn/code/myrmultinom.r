# @description
# simulate a single multinomial random variable
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @params
# n SCALAR, number of samples to take
# prob VECTOR length k, probabilities of respective outcomes
#
# @return
# results VECTOR length k, cumulative sample, respective to prob parameter

# DEPENDENCIES
# NONE

#my rmultinom random number generator. input is a vector of probabilities from the dir dis, output is a simulation from those probabilities
myrmultinom=function(n, prob){
  k = length(prob)
  u = runif(n)
  p = cumsum(prob)
  results = rep(0, k)
  for (x in u) {
    for (i in 1:k) {
      if (x < p[i]) {
        results[i] = results[i] + 1
        break()
      }
    }
  }
  return(results)
}

## END
