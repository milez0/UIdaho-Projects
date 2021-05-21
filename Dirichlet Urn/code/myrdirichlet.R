# @description
# function to sample from Dirichlet distribution with rejection using only uniform r.v.s.
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# n SCALAR, size of sample
# alpha VECTOR length l, shape parameter, initial (white, red, blue) balls in urn
# 
# @return
# x MATRIX n by l, n samples from Dirichlet(alpha) distribution

# DEPENDENCIES
# rgamma.ar

myrdirichlet=function(n,alpha){
  l=length(alpha)
  x=NULL
  aa=rep(NA, l)
  for(ii in 1:n){ #looping through alpha's creating list of random numbers from gamma dist
    #into vector of n* length of alpha
    for(i in 1:l){
      aa[i]=rgamma.ar(1,alpha[i])
    }
    x=rbind(x,aa)
  }
  row.names(x) = NULL
  sm <- rowSums(x)
  return(x/sm)#returning a vector of probabilties from the dir dist of n rows by width length of alpha all adding to 1
}

## END
