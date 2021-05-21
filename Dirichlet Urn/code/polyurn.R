# @description
# Function to perform the part a sampling
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# (alpha1, alpha2, alpha3) SCALAR, initial (white, red, blue) balls
# n SCALAR, number of draws from the urn
# 
# @return
# draw_results VECTOR, length 3, number of (white, red, blue) balls drawn from the urn


# DEPENDENCIES
# myrdirichlet
# myrmultinom


polyurn=function (alpha1,alpha2,alpha3,n){
  
  alpha=c(alpha1,alpha2,alpha3)#creating a vector of alpha's
  x=myrdirichlet(n,alpha) #inserting alpha vector and sample size into function from above.
  #output is matrix of length alpha by n
  d=length(alpha)#d variable with number of rows in alpha
  draw_results=NULL#creating blank matrix length alpha by n
  for (i in 1:n){
    draw_results=rbind(draw_results, myrmultinom(1, prob = x[i,])) #(size 1 for 1 ball per draw)
  }                                                   #simulating from multinomial dist with probabilities from dir dist.
  colnames(draw_results)=(c("w","r","b")) #adding ball colors to names of columns
  return(colSums(draw_results)) # returns data frame n rows by lenght alpha
}

## END
