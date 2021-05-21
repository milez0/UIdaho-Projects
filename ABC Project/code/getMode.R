
# @description
# Report mode. Use mean to make sure it is scalar output.
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# x numeric vector
# 
# @return
# y scalar, average of elements with most entries in x
# 

getMode = function(x) {
  tab = table(x)
  modes = as.numeric(names(tab[tab==max(tab)]))
  return(mean(modes))
}

# END
