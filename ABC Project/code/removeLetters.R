
# @description
# Strip down a string, removing all letters in a set from it.
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# str string, to be stripped
# toss char vector, letters to be removed from str
# 
# @return
# newstring string, str with all letters shared by toss removed
# 

removeLetters = function(str, toss) {
  newstring = str
  for (ch in toss) {
    newstring = paste(strsplit(newstring, ch)[[1]], sep = "", collapse = "")
  }
  return(newstring)
}

# END
