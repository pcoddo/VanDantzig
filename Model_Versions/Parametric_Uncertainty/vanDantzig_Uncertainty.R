###################################
# file: vanDantzig_Uncertainty.R
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
# Last modified: 1 October, 2015
###################################

# Set working directory
# setwd("~/Documents/Grad/SCRiM/vanDantzig/Model_Versions/Parametric_Uncertainty")

# Compile
rm(list = ls())
graphics.off()

library(compiler)
enableJIT(3)

# Set seed for reproducibility 
set.seed(1)

# Number of observations
n_obs = 10^4

# Prior parameter values - Section 6 "The Doubtful Constants," van Dantzig (1956)
source("Scripts/priors.R")
  # p_zero_p        = 0.0038      # Initial flood frequency (1/yr) with zero height increase
  # alpha_p         = 2.6         # Exponential flood frequency constant
  # V_p             = 1e+10 * 2   # Value of goods protected by dike (includes "factor of 2" for cost of human life)
  # delta_prime_p   = 0.02        # Discount rate (percent/year)
  # k_p             = 4.2e7       # Cost of heightening dikes by 1 meter
  # subs_rate       = 0.002       # Rate of land subsidence (meter/year)
  # sea_level_rate  = 0.008       # Rate of sea level rise (meter/year)

# Sample parameters with LHS function
# Creates data.frame with parameter PDFs
source("Scripts/LHS.R")

###################################
### van Dantzig problem setup ###
###################################

# Time horizon until next evaluation of dikes (years)
T = 75

# Time scale
beta_p = alpha_p * (subs_rate + sea_level_rate)

# Range of considerend dike heights (meters)
X = seq(0, 10, by=0.05)

# Time horizon in annual increments
time = seq(0, T, by=1)

# Define variables
p_exceed                   = array(NA, dim = c(length(X), n_obs))
costs                      = array(NA, dim = c(length(X), n_obs))
NPV_expected_losses        = array(NA, dim = c(length(X), n_obs))
EV_p_exceed_transient      = array(NA, dim = c(length(X), n_obs))
Total_flood_frequency      = array(NA, dim = c(length(X), n_obs))
total_costs                = array(NA, dim = c(length(X), n_obs))

discount_factor            = array(NA, dim = c(length(time), n_obs))

effective_height           = array(NA, dim = c(length(X), length(time), n_obs))
p_exceed_transient         = array(NA, dim = c(length(X),length(time), n_obs))
NPV_costs_flooding         = array(NA, dim = c(length(X),length(time), n_obs))

subsidence                 = array(NA, dim = c(length(time), n_obs))
sea_level_rise             = array(NA, dim = c(length(time), n_obs))

# Run model for 10,000 SOW
# Analyze for each considered Deike heightening (eq. 1 in paper)

    # Exceedance probability of current dike height
      p_exceed = t(sapply(1:length(X), function(i) {    
          Parameters$p_zero_p * exp(-(Parameters$alpha_p) * X[i])   }))
    
    # Land subsidence over length of time horizon
      subsidence = t(sapply(1:length(time), function(j) {
          Parameters$subs_rate * time[j]   }))
        
    # Sea level rise over length of time horizon
      sea_level_rise = t(sapply(1:length(time), function(j) {
          Parameters$sea_level_rate * time[j]    }))
        
    # Annual discounting factor as a function of time
      discount_factor = t(sapply(1:length(time), function(j) {
          1/(1+Parameters$delta_prime_p)^time[j]   }))
          
    # The effective dike height is the current height minus the 
    # combined effects of subsidence and sea-level rise
      effective_height[,,] = t(sapply(1:length(X), function(i) {
          t(sapply(1:length(time), function(j) {
              X[i] - subsidence[j,] - sea_level_rise[j,]    }))   }))
        
    # Annual flood frequency using old observations and new effective height 
    # (assumes stationary flooding frequency)
      p_exceed_transient[,,] = t(sapply(1:length(X), function(i) {
          t(sapply(1:length(time), function(j) {
              Parameters$p_zero_p * exp(-(Parameters$alpha_p) * effective_height[i,j,])   }))   }))
        
    # Net Present Value of the discounted expected annual losses (damages due to flooding in a given year)
      NPV_costs_flooding[,,] = t(sapply(1:length(X), function(i) {
          t(sapply(1:length(time), function(j) {
              p_exceed_transient[i,j,]*Parameters$V_p*discount_factor[j,]   }))   }))
      
    # The costs of flood protection (dike building) - increase linearly with respect to height
      costs = t(sapply(1:length(X), function(i) {
          Parameters$k_p * X[i]   }))
      
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
      
###################################
  
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

# Save Global Environment
save.image("Parametric_Uncertainty.RData")