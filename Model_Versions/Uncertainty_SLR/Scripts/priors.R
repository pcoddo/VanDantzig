###################################
# file: priors.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Prior parameter values for van Dantzig (1956) model
#################################### 

p_zero_p = 0.0038         # Initial flood frequency (1/yr) with zero height increase
alpha_p = 2.6             # Exponential flood frequency constant
V_p = 1e+10 * 2           # Value of goods protected by dike (includes "factor of 2" for cost of human life)
delta_prime_p = 0.02      # Discount rate (percent/year)
k_p = 4.2e7               # Cost of heightening dikes by 1 meter
subs_rate = 0.002         # Rate of land subsidence (meter/year)
sea_level_rate = 0.008    # Rate of sea level rise (meter/year)

# Create data frame of prior values
priors = data.frame(p_zero_p, alpha_p, V_p, delta_prime_p, k_p, subs_rate, sea_level_rate)