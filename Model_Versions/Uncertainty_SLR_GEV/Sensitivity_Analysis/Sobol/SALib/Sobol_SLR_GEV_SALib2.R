###################################
# file: Sobol_SLR_GEV_SALib.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
# 
# Distributed under GNU general public license
# No warranty
#
# Adapted from: cost-benefit-slr.R
# Authored by Ryan Sriver and Klaus Keller
# Pennsylvania State University
#
# Based on: van Dantzig D (1956). Economic Decision Problems for flood prevention. 
# Econometrica 24:276-287
###################################
# Last modified: 30 August, 2015
###################################

# Compile
rm(list = ls())
graphics.off()

library(compiler)
enableJIT(3)

# Set seed for reproducibility 
set.seed(1)

# Number of observations
n_obs = 10^4

# Read in Sobol Samples
source("Scripts/priors.R")
source("Scripts/LHS.R")
Sobol <- Parameters
#names(Sobol) <- c("V_p", "delta_prime_p", "k_p", "subs_rate", "a", "b", "c", "t.star", "c.star", "xi", "mu", "sigma") 

n_Sobol = length(Sobol[,1])

# Source sea level rise and storm surge modules
source("Scripts/Sobol_slr.R")
source("Scripts/Sobol_exceedance.R")

# Source baseline model priors
source("../../../Scripts/priors.R")

###################################
###  van Dantzig problem setup  ###
###################################

# Current year
year = 2015

# Time horizon until next evaluation of dikes (years)
T = 75

# Range of considerend dike heights (meters)
X = seq(0, 10, by=0.05)

# Initial dike height
H_0 = 4.25

# Time horizon in annual increments
time = seq(0, T, by=1)

# Initialize variables arrays
p_exceed                   = array(NA, dim = c(length(X), n_Sobol))
costs                      = array(NA, dim = c(length(X), n_Sobol))
NPV_expected_losses        = array(NA, dim = c(length(X), n_Sobol))
EV_p_exceed_transient      = array(NA, dim = c(length(X), n_Sobol))
Total_flood_frequency      = array(NA, dim = c(length(X), n_Sobol))
total_costs                = array(NA, dim = c(length(X), n_Sobol))

discount_factor            = array(NA, dim = c(length(time), n_Sobol))

effective_height           = array(NA, dim = c(length(X), length(time), n_Sobol))
p_exceed_transient         = array(NA, dim = c(length(X), length(time), n_Sobol))
NPV_costs_flooding         = array(NA, dim = c(length(X), length(time), n_Sobol))

subsidence                 = array(NA, dim = c(length(time), n_Sobol))

# Analyze for each considered Deike heightening (eq. 1 in paper)
    # Exceedance probability of current dike height (4.25 meters - Section 6)
#     p_exceed = t(sapply(1:length(X), function(i){
#       exceedance_prob(X[i] + H_0)   }))  
    
    # Land subsidence over length of time horizon
    subsidence = t(sapply(1:length(time), function(j) {
      Sobol$subs_rate * time[j]   }))
    
    # Sea level rise over length of time horizon
    sea_level_rise = t(sapply(1:length(time), function(j) {
      sea_level_global(Sobol$a, Sobol$b, Sobol$c, Sobol$c.star, (Sobol$t.star - year), j) /1000  }))#+ res.boot_proj[i,]
    
    # Annual discounting factor as a function of time
    discount_factor = t(sapply(1:length(time), function(j) {
      1/(1+Sobol$delta_prime_p)^time[j]   }))
    
    # The effective dike height is the current height minus the 
    # combined effects of subsidence and sea-level rise
    effective_height[,,] = t(sapply(1:length(X), function(i) {
      t(sapply(1:length(time), function(j) {
        X[i] - subsidence[j,] - sea_level_rise[j,]    }))   }))
    
    # Annual flood frequency using old observations and new effective heights 
    # (assumes stationary flooding frequency)

        # Find expected values of effective heights for each year in time horizon
        effective_means <- array(NA, dim = c(length(X), length(time)))
        effective_means <- t(sapply(1:length(X), function(i) {
          t(sapply(1:length(time), function(j) {
            mean(effective_height[i,j,]) + H_0    }))     }))
    
    for(j in 1:length(time))
    {  
      p_exceed_transient[,j,] = t(sapply(1:length(X), function(i){
        exceedance_prob(effective_means[i,j])   }))
    }
    
    # Net Present Value of the discounted expected annual losses (damages due to flooding in a given year)
    NPV_costs_flooding[,,] = t(sapply(1:length(X), function(i) {
      t(sapply(1:length(time), function(j) {
        p_exceed_transient[i,j,]*Sobol$V_p*discount_factor[j,]   }))   }))
    
    # The costs of flood protection (dike building) - increase linearly with respect to height
    costs = t(sapply(1:length(X), function(i) {
      Sobol$k_p * X[i]   }))
    
    # The total discounted expected losses are the sum of the discounted expected annual losses
    NPV_expected_losses = apply(NPV_costs_flooding, c(1,3), sum) 
    
    # Expected value of average annual flood frequency (mean of annual flood frequencies)
    EV_p_exceed_transient = apply(p_exceed_transient, c(1,3), mean)
    
    # The total flood frequency over the life-time of the project is the sum of the flood frequencies,
    # assuming independence, as in the original paper
    Total_flood_frequency = apply(p_exceed_transient, c(1,3), sum)
    
    # The total costs that depend on the dike height. This analysis neglects initial mobilization 
    # costs (I_0) in paper, as well as any costs extending beyond considered time horizon
    total_costs = t(sapply(1:length(X), function(i) {
      costs[i,] + NPV_expected_losses[i,]   }))
    


# Load baseline model (no parametric uncertainty) 
source("Scripts/Baseline_curves.R")

# Create data frame for 4 management objectives
  # 1) Minimize Total Costs
  # 2) Minimize Investment Costs
  # 3) Minimize NPV of Expected Damages
  # 4) Minimize Expected Annual Flood Frequency
Objectives.base <- data.frame(total_costs.base, costs.base, NPV_expected_losses.base, EV_p_exceed_transient.base)

# Uncertainty data frame - unlist dimensions and combine matrices into single vector
total_costs.v             = unlist(list(total_costs))
costs.v                   = unlist(list(costs))
NPV_expected_losses.v     = unlist(list(NPV_expected_losses))
EV_p_exceed_transient.v   = unlist(list(EV_p_exceed_transient))

Objectives <- data.frame(total_costs.v, costs.v, NPV_expected_losses.v, EV_p_exceed_transient.v)

# Load curves averaged across each state of the world
source("Scripts/Mean_curves.R")

# Create data frame for mean (expected) objective values
exp_objectives <- data.frame(total_costs[min_ind_mean,], costs[min_ind_mean,], NPV_expected_losses[min_ind_mean,], EV_p_exceed_transient[min_ind_mean,])

# Write table to text file for Sobol Sensitivity Analysis
write.table(exp_objectives, file = "objectiveValues.txt", sep = ' ', col.names = FALSE, row.names = FALSE)

# Save Global Environment
save.image("Output/SALib_SLR_GEV2.RData")