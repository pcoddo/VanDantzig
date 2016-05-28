###################################
# file: LHS.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Latin Hypercube Sampling of parameters
# generates data frame with PDFs
###################################

# Setup -------------------------------------------------------------------
require(lhs)

# LHS Function
fn.LHS <- function(n_obs, z, y, p_zero_p, alpha_p, V_p, delta_prime, k_p, subs_rate, sea_level_rate) {
  z <- randomLHS(n_obs, 7)
  y <- z
  y[,1] <- qlnorm(z[,1], log(p_zero_p), 0.25)
  y[,2] <- qnorm(z[,2], alpha_p, 0.1) 
  y[,3] <- qnorm(z[,3], V_p, 1e9)
  y[,4] <- qlnorm(z[,4], log(delta_prime_p), 0.1)
  y[,5] <- qnorm(z[,5], k_p, 4e6) #8e6
  y[,6] <- qlnorm(z[,6], log(subs_rate), 0.1) #0.5
  y[,7] <- qlnorm(z[,7], log(sea_level_rate), 0.1) #0.5 
  
  return(as.data.frame(y))
}


# Output ------------------------------------------------------------------

Parameters <- fn.LHS(n_obs, z, y, p_zero_p, alpha_p, V_p, delta_prime, k_p, subs_rate, sea_level_rate)
names(Parameters) <- c("p_zero_p", "alpha_p", "V_p", "delta_prime_p", "k_p", "subs_rate", "sea_level_rate")
