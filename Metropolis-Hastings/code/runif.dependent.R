# @description
# Generate a value uniformly distributed about a given value, but if generated outside given bounds, reflected across the bound.
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# x SCALAR, center value (min < x < max)
# min SCALAR, minimum value to generate
# max SCALAR, maximum value to generate
# width SCALAR, width about x to generate within
# 
# @return
# y SCALAR, value generated
# 

runif.dependent = function(x, min = 0, max = 10, width = .1){
  halfwidth = width/2
  x = runif(1, x-halfwidth, x+halfwidth)
  while (x < min | x > max) {
    if (x < min) {
      x = -x + 2*min
    } else {
      x = -x + 2*max
    }
  }
  return(x)
}

# END
