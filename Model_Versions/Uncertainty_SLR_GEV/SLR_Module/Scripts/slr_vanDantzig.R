###################################
# file: slr_vanDantzig.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Sea level rise module for van Dantzig analysis
# Based on rejection sampling framework:
# Lempert et al. 2012 - Characterizing Uncertain Sea Level Rise Projection
#################################### 

# Define function for global sea level rise
sea_level_global <- function(a,      # sea level anomaly [m] at t_0
                             b,      # initial rate      [m/a]
                             c,      # accelartion       [m/a^2]
                             c_star, # abrupt increase of rate [m/a]
                             t_star, # timing of abrupt rate increase
                             t) {
  sea_level_global <- a + b*t + c*(t^2) + c_star * ( (sign(t - t_star) + 1) / 2 ) * (t - t_star)
  
  return(sea_level_global)  
}

beta.dist = read.table("SLR_Module/Output/array_beta.txt", header = T)

#beta.dist = sapply(1:6, function(x){
#  sample(beta.dist[,x], n_obs, replace = FALSE)   })

beta.dist2 = beta.dist

beta.dist = beta.dist[sample(nrow(beta.dist), n_obs),]

beta.dist = as.data.frame(beta.dist)
colnames(beta.dist) = c("a","b","c","t.star","c.star","slr")


#slr_test <- array(NA, dim = c(85, length(beta.dist[,1])))

#for(i in 1:85){
#  slr_test[i,] = sea_level_global(beta.dist[,1], beta.dist[,2], beta.dist[,3], beta.dist[,5], (beta.dist[,4]-2015), i) #+ res.boot_proj[i,]
#}

### Plot
# source("Scripts/mycolors.R")
# slr_old = sea_level_rate * time
# expected <- apply(sea_level_rise, MARGIN = 1, mean)
# 
# pdf("../SLR Module/output/slr_compare.pdf", width = 6, height = 4.5)
# par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1)+0.1)
# 
# matplot(time, sea_level_rise[,runif(75, 1, 10000)], type = 'l', lty = 1, lwd = 0.5, col=rgb(0,0,0,0.5),
#          ylab = "Sea Level [m]", xlab = "Years",
#         xaxt = 'n')
# axis(1, at = seq(0, 75, 25), labels = seq(0, 75, 25))
# lines(time, slr_old, col = myred, lwd = 5)
# lines(time, expected, col = myblue, lwd = 5)
# 
# legend("topleft",
#        c("van Dantzig [1956]", "Rejection sample scenarios","Expected sea level rise"),
#        col = c(myred, "black", myblue),
#        lty = 1,
#        lwd = c(3, 1, 3),
#        bty = 'n')
# box(lwd = 1.5)
# 
# dev.off()
# 
# 
# 
# 
