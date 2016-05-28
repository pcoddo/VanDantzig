###################################
# file: exceedance_prob.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Generates survival function curves
# for exceedance probability estimations
#################################### 

library(fExtremes)
library(extRemes)

# Read in MCMC parameters
GEV_param <- read.table("Storm_Surge_Module/Output/param_full.txt", sep = '\t', header = T)

# Resample distributions to ensure 10,000 samples from within credible interval
mu <- GEV_param$mu
xi <- GEV_param$xi
sigma <- exp(GEV_param$logsigma)

# Recombine into new data frame
GEV_param <- data.frame(xi, mu, sigma)
rm(mu, xi, sigma)

# Determine exceedance probabilities by generating survival curves for each GEV parameter set
q = seq(0,1, length.out = 10^4+2)
length_param = length(GEV_param[,1])

gev <- sapply(1:length_param, function(x){
  qgev(q, GEV_param$xi[x], GEV_param$mu[x], GEV_param$sigma[x])
})

gev <- gev[2:(length(gev[,1])-1),]
q <- seq(0,1, length.out = length(gev[,1]))

min_es <- sapply(1:length_param, function(x){
  min(gev[,x]/100)
})

max_es <- sapply(1:length_param, function(x){
  max(gev[,x]/100)
})

p_exceed <- mat.or.vec(length_param, 1)
p_index <- mat.or.vec(length_param, 1)

# Function for Initial Exceedance Frequency
exceedance_prob <- function(X)
  {

    p_exceed <- mat.or.vec(length_param, 1)
    
    p_exceed <- sapply(1:length_param, function(i) {
      pevd((X * 100), scale = GEV_param$sigma[i], shape = GEV_param$xi[i], loc = GEV_param$mu[i])
    })
    
    return(p_exceed)
  }
#     p_exceed <- sapply(1:length_param, function(i) {
#       if(X <= min_es[i]) {
#         return(1)
#       } 
#       
#       if(X >= max_es[i]) {
#         return(0)
#       } 
#       
#       if(X > min_es[i] && X < max_es[i]) {
#         return(1-q[which.min(abs(gev[,i]/100 - X))])
#       }
#   })
    
    #return(p_exceed)
    
#}