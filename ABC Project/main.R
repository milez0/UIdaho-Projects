
# @description
# ABC decipher given distorted bit arrays.
# Maintains only the letters with more than 32 1s in order.
#
# @author
# Raymond Mezazem
# Nick Stutzman
# Miles Keppler
# 
# @param
# NONE
# 
# @return
# NONE
# 
# -------------------------------------------
# 
# PATHS
# user should change baseDir to location of main.R

baseDir = "D:/Bork/Documents/UIdaho/STAT 565/Assignments/Exam 4"
scriptDir = file.path(baseDir, "code")
outputDir = file.path(baseDir, "data")

# LIBRARIES and FUNCTIONS

source(file.path(scriptDir, "removeLetters.R"))
source(file.path(scriptDir, "getMode.R"))

# SEEDS
set.seed(102)

# INPUT PARAMS
load(file.path(outputDir, "ABCPractice.RData")) # KeyObs
string = "OPIEHAKLESSXFYHBWWEEFLKMRFJAIICNSAIFNDFAJNJVSEUFBEHHRFEQUAQ"
theta = c(.01, .99, .99, .97, .02)
N = 2e4

# for part B :
M = 15
var.adjuster = 200/sqrt(2)^(0:(M-1))

# set R < 20 to keep runtime under an hour (for my own sanity)
R = 10

# OUTPUT PARAMS
cipher = matrix(NA, nrow = 26, ncol = N)
ciph.bool = rep(NA, 26)

VARS = matrix(NA, nrow = M, ncol = 5)
ALPHA = matrix(NA, nrow = M, ncol = 5)
BETA = matrix(NA, nrow = M, ncol = 5)

RESULTS = data.frame(stringsAsFactors = F)
RESULTS.BOOL = matrix(NA, nrow = R, ncol = M)

# CODE

#### A ####

# each row is a posterior distribution
cipher = matrix(NA, nrow = 26, ncol = N)
for (i in 1:26) {
  # placeholder to avoid a few sums
  ONES = sum(KeyObs[i,])
  for (j in 1:N) {
    ones = ONES
    for (t in theta) {
      ones = rbinom(1, 64-ones, t) + rbinom(1, ones, 1-t)
    }
    cipher[i,j] = ones
  }
}
## Some notes :
##
## Generate by compounding binomial distributions
## Detail of interest : true parameter > 32 ?
## 

ciph.bool = apply(cipher, 1, getMode) > 32

# letters to remove :
# throw the mean function in incase there are multiple modes
toss = LETTERS[!ciph.bool]

# recover string
newstring = removeLetters(string, toss)

# answer : newstring =  ABRACADABRA

#### B ####
# beta mode in handout is incorrect.
# (alpha - 1) / (alpha + beta - 2)
# not goint to bother solving for mode/variance. it seems hard.
# just having small variance will have to do.

for (k in 1:M) {
  adj = var.adjuster[k]
  # use adj (just a leverage variable) to
  # make up some alpha, beta that fit theta (mode)
  alpha = theta*adj + 1
  beta = (alpha-1)/theta - alpha + 2
  # variance calculated (for diagnostic / fun)
  v = (alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
  VARS[k,] = v
  ALPHA[k,] = alpha
  BETA[k,] = beta
}

for (l in 1:R) {
  system.time({
  CIPH = matrix(NA, nrow = M, ncol = 26)
  for (k in 1:M) {
    
    alpha = ALPHA[k,]
    beta = BETA[k,]
    
    # simulate theta*
    theta.s = rbeta(5, alpha, beta)
    # simulate x*
    
    cipher.s = matrix(NA, nrow = 26, ncol = N)
    for (i in 1:26) {
      # placeholder to avoid a few sums
      ONES = sum(KeyObs[i,])
      for (j in 1:N) {
        ones = ONES
        for (t in theta.s) {
          ones = rbinom(1, 64-ones, t) + rbinom(1, ones, 1-t)
        }
        cipher.s[i,j] = ones
      }
    }
    CIPH[k,] = apply(cipher.s, 1, getMode)
  }
  
  results = rep("", M)
  results.bool = rep(NA, M)
  for (k in 1:M) {
    bool = CIPH[k,] > 32
    results[k] = removeLetters(string, LETTERS[!bool])
    results.bool[k] = sum(ciph.bool == bool)
  }
  RESULTS = rbind(RESULTS, results, stringsAsFactors = F)
  RESULTS.BOOL[l,] = results.bool
  })
}
colnames(RESULTS) = NULL

## 
## Results
##
## Ballpark estimate that variance less than .005 tends to work out.
## by .025 we lose the ability to use the cipher at all.
##
## Certainly could get better estimates by solving the the parameters
## of the beta distribution with respect to the mode and variance.
##
## Also without that step, we could use numerical methods to bring the variances
## closer together, but we chose not to.
##

save(string, RESULTS, ciph.bool, ALPHA, BETA, theta, N, M, R, file = file.path(outputDir, "cipher.data"))

# END

