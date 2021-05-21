
# @description
# Sample from the multinomial-Dirichlet distribution to represent a 
# simulation of draws from a multivariate Polya urn model.
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

baseDir = "D:/Bork/Documents/UIdaho/STAT 565/Assignments/Exam 1/Practice2"
scriptDir = file.path(baseDir, "code")
outputDir = file.path(baseDir, "data")

# LIBRARIES and FUNCTIONS

source(file.path(scriptDir, "myrdirichlet.R")) # Dirichlet random variable simulation
source(file.path(scriptDir, "myrmultinom.R")) # Multinomial r.v. simulation
source(file.path(scriptDir, "rgamma.ar.R")) # Gamma r.v. simulation
source(file.path(scriptDir, "polyurn.R")) # Primary function for part A
source(file.path(scriptDir, "polyurn2.R"))

# SEEDS

set.seed(51)

# -------------------------------------------
# INPUT PARAM
# part a 
# default parameters
alpha = c(w = 1, r = 5, b = 2)
n = 100
# part b
# non-integer parameters
alpha2 = c(1.5, 5.5, 2.5, 3.5) 

parameters.in = list(alpha, n, alpha2)

# OUTPUT PARAM

results = NA
results2 = NA

# CODE

results=polyurn(alpha[1],alpha[2],alpha[3],n) #need 3 alpha's and sample size
results2=polyurn2(alpha2,100)#alpha vector and sample size are needed

# OUTPUT

data.out = list(results, results2)
names(parameters.in) = c('alpha', 'n', 'alpha2')
names(data.out) = c('results', 'results2')
save(parameters.in, file = file.path(outputDir, "parameters.dat"))
save(data.out, file = file.path(outputDir, "data.dat"))

# END
