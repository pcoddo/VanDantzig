###################################################
# file: vanDantzig_baseline_learnings.R
#
# - van Dantzig (1956) flood risk model
# - Includes analytical solution and numerical analysis with uncertainty
###################################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
#
# Adapted from cost-benefit-slr.R
# Authored by: Ryan Sriver and Klaus Keller
# rsriver@illinois.edu, klaus@psu.edu
# Pennsylvania State University
#
# Distributed under the GNU general public license
# No warranty
#
# Based on: van Dantzig D (1956) Economic Decision Problems for flood prevention. 
# Econometrica 24:276-287
###################################################
# Last changes: Sept 3, 2015 (PCO)
###################################################

# Set working directory
setwd("~/Documents/Grad/SCRiM/vanDantzig/Model_Versions/Baseline_Model")

# Clear environment and figures
rm(list = ls())
graphics.off()

# Load required libraries
library(zoo)
library(lubridate)

# Load tide gauge data for updated sea level informations
load("../Uncertainty_SLR_GEV/Storm_Surge_Module/Output/Delfzijl_sea_level")

# Aggregate annual mean values
as.year <- function(x) {floor(as.numeric(as.yearmon(x)))}
year.mean <- aggregate(sl, as.year, mean)

# Find updated linear sea level rate through linear regression
sea_level.lm <- lm(coredata(year.mean)~index(year.mean))
sea_level_new <- summary.lm(sea_level.lm)$coefficient[2]

  # Divide by 100 to convert slope to meter/year
  sea_level_new <- sea_level_new / 100

# Load original prior parameter values: 
# Conservative estimates found in Section 6 - "The Doubtful Constants," van Dantzig (1956)
source("Scripts/priors.R")
  # p_zero_p = 0.0038         # Initial flood frequency (1/year) with zero height increase
  # alpha_p = 2.6             # Exponential flood frequency constant
  # V_p = 2 * 1e+10           # Value of goods protected by dike (includes "factor of 2" for cost of human life - Section 7)
  # delta_prime_p = 0.02      # Discount rate (percent/year)
  # k_p = 4.21e7              # Cost of heightening dikes by 1 meter
  # subs_rate = 0.002         # Rate of land subsidence (meter/year)
  # sea_level_rate = 0.008    # Rate of sea level rise (meter/year)

sea_level_rate <- sea_level_new

# Time horizon until next evaluation of dikes (years)
T = 75

# Time scale
beta_p = alpha_p * 1

# Initial dike height
H_0 = 4.25

###################################################
###          van Dantzig problem setup          ###
###################################################

### Case 1: Perform economic analysis without uncertainty

# Optimal dike heightening, X (equation 6 in paper), analytical solution:
C = (100 * p_zero_p * V_p * alpha_p) / ((delta_prime_p * 100-beta_p) * k_p) *
    (1 - exp(-(delta_prime_p * 100-beta_p) * T/100)) / 
    (1 - exp(-delta_prime_p * 100 * T / 100));

X_analytical = 1/alpha_p * log(C)

# New dike height without uncertainty:
  print(X_analytical + H_0)



### Case 2: Numerical Analysis with uncertainty

# Range of considerend dike heights (meters)
X = seq(0, 10, by=0.05)

# Time horizon in annual increments
time = seq(0, T, by=1)

# Define variables and intialize with NA values
p_exceed                   = array(NA, dim=c(length(X)))
costs                      = array(NA, dim=c(length(X)))
NPV_expected_losses        = array(NA, dim=c(length(X)))
EV_p_exceed_transient      = array(NA, dim=c(length(X)))
Total_flood_frequency      = array(NA, dim=c(length(X)))
total_costs                = array(NA, dim=c(length(X)))

discount_factor            = array(NA, dim=c(length(time)))

effective_height           = array(NA, dim=c(length(X), length(time)))
p_exceed_transient         = array(NA, dim=c(length(X), length(time)))
NPV_costs_flooding         = array(NA, dim=c(length(X), length(time)))

subsidence                 = subs_rate * time
sea_level_rise             = sea_level_rate * time

# Analyze for each considered Deike heightening 
# Loop over each dike height (X), and for each year in time horizon

for(i in 1:length(X)) {
  
  # Exceedance probability of current dike height (equation 1)
  p_exceed[i] = p_zero_p*exp(-alpha_p*X[i])
  
      for (j in 1:length(time)) {
  	
    	# Analze for each year of the initial time period
  	  year = j-1
  	
  	  # Annual discounting factor as a function of time
  	  discount_factor[j] = 1/(1+delta_prime_p)^year
  
  	  # The effective dike height is the current height minus the 
  	  # combined effects of land subsidence and sea-level rise
  	  effective_height[i,j] = X[i]-subsidence[j]-sea_level_rise[j]
  
  	  # Annual flood frequency using old observations and new effective height 
  	  # (assumes stationary flooding frequency)
  	  p_exceed_transient[i,j] = p_zero_p*exp(-alpha_p*effective_height[i,j])
  
  	  #The net present value of the losses per year are the product of the
  	  #frequency of flooding per year, the damages per flood, and the discount factor.
  	  NPV_costs_flooding[i,j] = p_exceed_transient[i,j]*V_p*discount_factor[j]
      }

  # The costs of flood protection (dike building) - increase linearly with respect to height
  costs[i] = k_p*X[i]

  # The total discounted expected losses are the sum of the discounted expected annual losses
  NPV_expected_losses[i] = sum(NPV_costs_flooding[i,])
   
  # Expected value of average annual flood frequency (mean of annual flood frequencies)
  EV_p_exceed_transient[i] = mean(p_exceed_transient[i,])

  # The total flood frequency over the life-time of the project is the sum of the flood frequencies,
  # assuming independence, as in the original paper
  Total_flood_frequency[i] = sum(p_exceed_transient[i,])

  # The total costs that depend on the dike height. This analysis neglects initial mobilization 
  # costs (I_0) in paper, as well as any costs extending beyond considered time horizon
  total_costs[i] = costs[i]+NPV_expected_losses[i]

}


# Find the minimum cost along the Total Cost curve
min_ind = seq(along=total_costs)[total_costs == min(total_costs)]
min_cost_X = X[min_ind]
  print(min_cost_X)
  
# Find optimal dike height under uncertainty
X_optimal = min_cost_X + H_0
  print(X_optimal)

# Find de-minimis risk defense (minimum flood frequency threshold)
de_minimis_risk = 1e-06      # Corresponds to 1 flood in 1,000,000 years 

# Find dike height increase corresponding to de-mininis risk defense
# This is the minimum height at which total flood frequency is below a threshold
de_minimis_risk_ind = min(which(Total_flood_frequency <= de_minimis_risk))
min_cost_de_minimis_X = X[de_minimis_risk_ind]
  print(min_cost_de_minimis_X)
  
  
  
  
  
#####################################################
###                 Plot Results                  ###
#####################################################
  
# Van Dantzig cost-benefit analysis - optimal dike height vs. minimum risk height
# Compare with optimal height found in paper (H_0 + 2.48m = 6.73m)
  
pdf("Figures/optimal_dike_learning.pdf", width = 6, height = 4.5)
par(mar = c(4,4,1,1)+0.1, oma = c(0,0,0,0)+0.1)

plot(X, NPV_expected_losses, type = 'l', 
     col = 'red', 
     lwd = 3, 
     ylim = c(0,7e+08),
     xlab = expression(bold("Dike height increase [m]")), 
     ylab = expression(bold('Expected cost [Guilders]'))
)
abline(v = 2.48, col = "gray", lwd = 3)
lines(X, costs, col = 'blue', lwd = 3)
lines(X, total_costs, col = 'black', lwd = 4, lty = 2)
points(min_cost_X, total_costs[min_ind], pch = 20, cex = 2)
points(min_cost_de_minimis_X, total_costs[de_minimis_risk_ind], pch = 18, cex = 2, col = "orange")

text(min_cost_X, 
     total_costs[min_ind]*1.25,
     labels = min_cost_X)

text(min_cost_de_minimis_X, 
     total_costs[de_minimis_risk_ind]*1.15,
     labels = min_cost_de_minimis_X)

legend("topright", 
       c("Expected Losses", 
         "Investment Costs", 
         "Total Costs", 
         "Minimum Cost", 
         "Minimum Risk", 
         "van Dantzig [1956] optimal"),
       lty = c(1, 1, 2, NA, NA, 1),
       lwd = c(3, 3, 3, NA, NA, 3),
       pch = c(NA, NA, NA, 20, 18, NA),
       col = c("red", "blue", "black", "black", "orange", "gray"),
       pt.cex = 1.25,
       bty = 'n',
       inset = c(0.01, 0.01)
)

box(lwd = 1.5)

dev.off()


# Sea level plot
pdf("Figures/sea_level_learn.pdf", width= 6, height = 4.5)
par(mar = c(4,4,1,1)+0.1, oma = c(0,0,0,0)+0.1)

plot(year.mean, lwd = 2,
     col = "gray",
     xlab = "Year",
     ylab = "Mean sea level [cm]")
box(lwd = 1.5)
abline(sea_level.lm, lty = 2)
text(1980, -8, labels = bquote('m ='~.(round(sea_level_new, digits = 5))))

dev.off()  