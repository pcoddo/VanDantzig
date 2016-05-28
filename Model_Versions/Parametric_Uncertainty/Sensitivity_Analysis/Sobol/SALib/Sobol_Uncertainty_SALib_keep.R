###################################
# file: Sobol_Uncertainty_SALib.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Sobol Analysis for parameter sensitivity
# Based on The Sensitivity Analysis Library (SALib):
# https://github.com/SALib/SALib
#################################### 

# Compile
rm(list = ls())
graphics.off()

library(compiler)
enableJIT(3)

# Set seed for reproducibility 
set.seed(1)

# Number of observations
n_obs = 10^4

# Set prior parameter values - Section 6 "The Doubtful Constants," van Dantzig (1956)
source("../../../Scripts/priors.R")
 # p_zero_p        = 0.0038      # Initial flood frequency (1/yr) with zero height increase
 # alpha_p         = 2.6         # Exponential flood frequency constant
 # V_p             = 1e+10 * 2   # Value of goods protected by dike (includes "factor of 2" for cost of human life)
 # delta_prime_p   = 0.02        # Discount rate (percent/year)
 # k_p             = 4.2e7       # Cost of heightening dikes by 1 meter
 # subs_rate       = 0.002       # Rate of land subsidence (meter/year)
 # sea_level_rate  = 0.008       # Rate of sea level rise (meter/year)

# Read in Sobol Samples
#source("../../../Scripts/LHS.R")
#Sobol <- Parameters
Sobol <- read.table("sobolParameterSets.txt", sep = ' ', header = FALSE)
names(Sobol) <- names(priors)

n_Sobol = length(Sobol[,1])

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
p_exceed                = array(NA, dim = c(length(X), n_Sobol))
costs                   = array(NA, dim = c(length(X), n_Sobol))
NPV_expected_losses     = array(NA, dim = c(length(X), n_Sobol))
EV_p_exceed_transient   = array(NA, dim = c(length(X), n_Sobol))
Total_flood_frequency   = array(NA, dim = c(length(X), n_Sobol))
total_costs             = array(NA, dim = c(length(X), n_Sobol))

discount_factor         = array(NA, dim = c(length(time), n_Sobol))
effective_height        = array(NA, dim = c(length(X), length(time), n_Sobol))
p_exceed_transient      = array(NA, dim = c(length(X), length(time), n_Sobol))
NPV_costs_flooding      = array(NA, dim = c(length(X), length(time), n_Sobol))

subsidence              = array(NA, dim = c(length(time), n_Sobol))
sea_level_rise          = array(NA, dim = c(length(time), n_Sobol)) 

# Analyze for each considered Deike heightening (eq. 1 in paper)
    # Exceedance probability of current dike height
    p_exceed = t(sapply(1:length(X), function(i) {    
      Sobol$p_zero_p * exp(-(Sobol$alpha_p) * X[i])   }))
    
    # Land subsidence over length of time horizon
    subsidence = t(sapply(1:length(time), function(j) {
      Sobol$subs_rate * time[j]   }))
    
    # Sea level rise over length of time horizon
    sea_level_rise = t(sapply(1:length(time), function(j) {
      Sobol$sea_level_rate * time[j]    }))
    
    # Annual discounting factor as a function of time
    discount_factor = t(sapply(1:length(time), function(j) {
      1/(1+Sobol$delta_prime_p)^time[j]   }))
    
    # The effective dike height is the current height minus the 
    # combined effects of subsidence and sea-level rise
    effective_height[,,] = t(sapply(1:length(X), function(i) {
      t(sapply(1:length(time), function(j) {
        X[i] - subsidence[j,] - sea_level_rise[j,]    }))   }))
    
    # Annual flood frequency using old observations and new effective height 
    # (assumes stationary flooding frequency)
    p_exceed_transient[,,] = t(sapply(1:length(X), function(i) {
      t(sapply(1:length(time), function(j) {
        Sobol$p_zero_p * exp(-(Sobol$alpha_p) * effective_height[i,j,])   }))   }))
    
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

###################################

# Baseline model data frame for 4 Objectives
# Load baseline model
source("Scripts/Baseline_curves.R")

Objectives.base <- data.frame(total_costs.base, costs.base, NPV_expected_losses.base, EV_p_exceed_transient.base)

# Load curves averaged for each dike height, X:
source("Scripts/Mean_curves.R")

# Create data frame for mean (expected) objective values
exp_objectives <- data.frame(total_costs[min_ind_mean,], costs[min_ind_mean,], NPV_expected_losses[min_ind_mean,], EV_p_exceed_transient[min_ind_mean,])

# Write table to text file for Sobol Sensitivity Analysis
write.table(exp_objectives, file = "objectiveValues.txt", sep = ' ', col.names = FALSE, row.names = FALSE)
