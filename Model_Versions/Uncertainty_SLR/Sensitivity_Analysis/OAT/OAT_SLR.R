###################################
# file: OAT_SLR.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# One-at-a-time sensitivity analysis
# for van Dantzig (1956) model
#################################### 

# Set working directory
setwd("~/Documents/Grad/SCRiM/vanDantzig/Model_Versions/Uncertainty_SLR/Sensitivity_Analysis/OAT")

# Compile
rm(list = ls())
graphics.off()

library(compiler)
enableJIT(3)

# Number of observations
n_obs = 10^4

# Set prior parameter values - Section 6 "The Doubtful Constants," van Dantzig (1956)
source("../../Scripts/priors.R")

# Source parameters for sea level rise and storm surge modules
source("Scripts/sample.R")
source("Scripts/slr_OAT.R")

Parameter_PDF <- Parameters 

# Create vector to change parameters from 1% to 99% quantile of PDF
quantile_prior <- seq(0.01, 0.99, by = 0.01)

# van Dantzig problem setup:
# Time horizon until next evaluation of dikes (years)
T = 75

# Initial dike height
H_0 = 4.25

# Current year
year = 2015

# Fix dike height at "optimal" level from baseline model
X = 2.35

# considered time horizon, in annual increments
time = seq(0,T,by=1)

# Define variables arrays
p_exceed                = array(NA, dim = c(length(X), length(quantile_prior)))
costs                   = array(NA, dim = c(length(X), length(quantile_prior)))
NPV_expected_losses     = array(NA, dim = c(length(X), length(quantile_prior)))
EV_p_exceed_transient   = array(NA, dim = c(length(X), length(quantile_prior)))
Total_flood_frequency   = array(NA, dim = c(length(X), length(quantile_prior)))
total_costs             = array(NA, dim = c(length(X), length(quantile_prior)))

discount_factor         = array(NA, dim = c(length(time), length(quantile_prior)))

effective_height        = array(NA, dim = c(length(X), length(time), length(quantile_prior)))
p_exceed_transient      = array(NA, dim = c(length(X), length(time), length(quantile_prior)))
NPV_costs_flooding      = array(NA, dim = c(length(X), length(time), length(quantile_prior)))

subsidence              = array(NA, dim = c(length(time), length(quantile_prior)))
sea_level_rise          = array(NA, dim = c(length(time), length(quantile_prior))) 

# Create uniform arrays for all parameters based on expected value of priors
  # Load posterior modes for GEV parameters

p_zero_p        = array(priors$p_zero_p, length(quantile_prior))
alpha_p         = array(priors$alpha_p, length(quantile_prior))
V_p             = array(priors$V_p, length(quantile_prior))
delta_prime_p   = array(priors$delta_prime_p, length(quantile_prior))
k_p             = array(priors$k_p, length(quantile_prior))
subs_rate       = array(priors$subs_rate, length(quantile_prior))
a               = array(mean(Parameters$a), length(quantile_prior))
b               = array(mean(Parameters$b), length(quantile_prior))
c               = array(mean(Parameters$c), length(quantile_prior))
t.star          = array(mean(Parameters$t.star), length(quantile_prior))
c.star          = array(mean(Parameters$c.star), length(quantile_prior))

Parameters <- data.frame(p_zero_p, alpha_p, V_p, delta_prime_p, k_p, subs_rate, a, b, c, t.star, c.star)

# Vector for 4 objectives 
total_costs_OAT             = array(NA, dim = c(length(quantile_prior), length(Parameters)))
costs_OAT                   = array(NA, dim = c(length(quantile_prior), length(Parameters)))
NPV_expected_losses_OAT     = array(NA, dim = c(length(quantile_prior), length(Parameters)))
EV_p_exceed_transient_OAT   = array(NA, dim = c(length(quantile_prior), length(Parameters)))

# Run model for by holding all parameters constant and changing one at a time
for(n in 1:length(Parameters)){

  p_zero_p        = array(priors$p_zero_p, length(quantile_prior))
  alpha_p         = array(priors$alpha_p, length(quantile_prior))
  V_p             = array(priors$V_p, length(quantile_prior))
  delta_prime_p   = array(priors$delta_prime_p, length(quantile_prior))
  k_p             = array(priors$k_p, length(quantile_prior))
  subs_rate       = array(priors$subs_rate, length(quantile_prior))
  a               = array(mean(Parameters$a), length(quantile_prior))
  b               = array(mean(Parameters$b), length(quantile_prior))
  c               = array(mean(Parameters$c), length(quantile_prior))
  t.star          = array(mean(Parameters$t.star), length(quantile_prior))
  c.star          = array(mean(Parameters$c.star), length(quantile_prior))
  
  Parameters <- data.frame(p_zero_p, alpha_p, V_p, delta_prime_p, k_p, subs_rate, a, b, c, t.star, c.star)
  
  # Create vector to change single parameter from 1% to 99% quantile
  Parameters[n] <- sapply(1:length(quantile_prior), function(x)
  {
    quantile(Parameter_PDF[,n], probs = quantile_prior[x])
  })
  

    # Analyze for each considered Deike heightening (eq. 1 in paper)
    # Exceedance probability of current dike height
    p_exceed = t(sapply(1:length(X), function(i) {    
      Parameters$p_zero_p * exp(-(Parameters$alpha_p) * X[i])   }))
    
    # Land subsidence over length of time horizon
    subsidence = t(sapply(1:length(time), function(j) {
      Parameters$subs_rate * time[j]   }))
    
    # Sea level rise over length of time horizon
    sea_level_rise = t(sapply(1:length(time), function(j) {
      sea_level_global(Parameters$a, Parameters$b, Parameters$c, Parameters$c.star, (Parameters$t.star - year), j) /1000  }))#+ res.boot_proj[i,]
    
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
  
# Find the index of the EUM point (minimum along total costs)
total_costs_OAT[,n] <- total_costs
costs_OAT[,n] <- costs
NPV_expected_losses_OAT[,n] <- NPV_expected_losses
EV_p_exceed_transient_OAT[,n] <- EV_p_exceed_transient

  
}

colnames(total_costs_OAT)             <- names(Parameters)
colnames(costs_OAT)                   <- names(Parameters)
colnames(NPV_expected_losses_OAT)     <- names(Parameters)
colnames(EV_p_exceed_transient_OAT)   <- names(Parameters)

#################################### 
# Plot results
#################################### 
library(RColorBrewer)
OAT_col <- brewer.pal(11, "Paired")

# Objective plots
pdf(file = "Figures/OAT_Objectives.pdf", width = 8, height = 8)
par(oma = c(0,0,0,0)+0.1, mar = c(5,5,2,1)+0.1, mfrow = c(2,2))

matplot(quantile_prior, (costs_OAT/1e+06), type = 'l', lty = 1, lwd = 2, 
        col = c("light gray", "light gray", "light gray", "light gray", OAT_col[5], "light gray", "light gray", "light gray", "light gray", "light gray", "light gray"),
        xlab = "Quantile of prior (%)",
        ylab = "Costs (million Guilders)",
        xaxt='n',
        las = 0)
axis(side = 1, at = c(0.01, 0.25, 0.5, 0.75, 0.99), labels = c(1, 25, 50, 75, 99))
abline(v = 0.5, lty = 2)
box(lwd = 1.5)
mtext("A. Investment Costs", font = 2, side = 3, line = 0.25, at = -0.03, adj = c(0,0))

legend("topleft",
       names(Parameters),
       ncol = 2,
       pch = 22, 
       bty = 'n',
       pt.bg = c("light gray", "light gray", "light gray", "light gray", OAT_col[5], "light gray", "light gray", "light gray", "light gray", "light gray", "light gray"),
       col = c("dark gray", "dark gray", "dark gray", "dark gray", "black", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray"),
       text.col = c("dark gray", "dark gray", "dark gray", "dark gray", "black", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray"),
       cex = 1,
       pt.cex = 1.5,
       inset = c(0.01, -0.02))

matplot(quantile_prior, (NPV_expected_losses_OAT/1e+06), type = 'l', lty = 1, lwd = 2, 
        col = c(OAT_col[1:4], "light gray", OAT_col[6:11]),
        xlab = "Quantile of prior (%)",
        ylab = "Costs (million Guilders)",
        xaxt='n',
        las = 0)
axis(side = 1, at = c(0.01, 0.25, 0.5, 0.75, 0.99), labels = c(1, 25, 50, 75, 99))
abline(v = 0.5, lty = 2)
box(lwd = 1.5)
mtext("B. NPV: Damages", font = 2, side = 3, line = 0.25, at = -0.03, adj = c(0,0))

legend("topleft",
       names(Parameters),
       ncol = 2,
       pch = 22, 
       bty = 'n',
       pt.bg = c(OAT_col[1:4], "light gray", OAT_col[6:11]),
       col = c("black", "black", "black", "black", "dark gray", "black", "black", "black", "black", "black", "black"),
       text.col = c("black", "black", "black", "black", "dark gray", "black", "black", "black", "black", "black", "black"),
       cex = 1,
       pt.cex = 1.5,
       inset = c(0.01, -0.02))

matplot(quantile_prior, (EV_p_exceed_transient_OAT), type = 'l', lty = 1, lwd = 2, 
        col = c(OAT_col[1:2], "light gray", "light gray", "light gray", OAT_col[6:11]),
        xlab = "Quantile of prior (%)",
        ylab = "Expected flood frequency (1/yr)",
        xaxt='n',
        las = 0)
axis(side = 1, at = c(0.01, 0.25, 0.5, 0.75, 0.99), labels = c(1, 25, 50, 75, 99))
abline(v = 0.5, lty = 2)
box(lwd = 1.5)
mtext("C. Reliability", font = 2, side = 3, line = 0.25, at = -0.03, adj = c(0,0))

legend("topleft",
       names(Parameters),
       ncol = 2,
       pch = 22, 
       bty = 'n',
       pt.bg = c(OAT_col[1:2], "light gray", "light gray", "light gray", OAT_col[6:11]),
       col = c("black", "black", "dark gray", "dark gray", "dark gray", "black", "black", "black", "black", "black", "black"),
       text.col = c("black", "black", "dark gray", "dark gray", "dark gray", "black", "black", "black", "black", "black", "black"),
       cex = 1,
       pt.cex = 1.5,
       inset = c(0.01, -0.02))

matplot(quantile_prior, (total_costs_OAT/1e+06), type = 'l', lty = 1, lwd = 2, 
        col = OAT_col,
        xlab = "Quantile of prior (%)",
        ylab = "Costs (million Guilders)",
        xaxt='n',
        las = 0)
axis(side = 1, at = c(0.01, 0.25, 0.5, 0.75, 0.99), labels = c(1, 25, 50, 75, 99))
abline(v = 0.5, lty = 2)
box(lwd = 1.5)
mtext("D. NPV: Total Costs", font = 2, side = 3, line = 0.25, at = -0.03, adj = c(0,0))

legend("topleft",
       names(Parameters),
       ncol = 2,
       pch = 22, 
       bty = 'n',
       pt.bg = OAT_col,
       col = "black",
       cex = 1,
       pt.cex = 1.5,
       inset = c(0.01, -0.02))

dev.off()

############################
### Tornado diagram

# Determine total parameter ranges for each Objective
  costs_range <- sapply(1:length(Parameters), function(x){
    max(costs_OAT[,x]) - min(costs_OAT[,x])
  })
  
  NPV_expected_losses_range <- sapply(1:length(Parameters), function(x){
    max(NPV_expected_losses_OAT[,x]) - min(NPV_expected_losses_OAT[,x])
  })
  
  EV_p_exceed_transient_range <- sapply(1:length(Parameters), function(x){
    max(EV_p_exceed_transient_OAT[,x]) - min(EV_p_exceed_transient_OAT[,x])
  })
  
  total_costs_range <- sapply(1:length(Parameters), function(x){
    max(total_costs_OAT[,x]) - min(total_costs_OAT[,x])
  })

names(costs_range) <- names(Parameters)
names(NPV_expected_losses_range) <- names(Parameters)
names(EV_p_exceed_transient_range) <- names(Parameters)
names(total_costs_range) <- names(Parameters)

# Determine percentage of total range above and below 50% quantile (center point)
# Below (quantile 50 - quantile 1)
  costs_range_minus <- sapply(1:length(Parameters), function(x) {
    abs((costs_OAT[50,x] - costs_OAT[1,x]) / costs_range[x])
  })
  
  NPV_expected_losses_range_minus <- sapply(1:length(Parameters), function(x) {
    abs((NPV_expected_losses_OAT[50,x] - NPV_expected_losses_OAT[1,x]) / NPV_expected_losses_range[x])
  })
  
  EV_p_exceed_transient_range_minus <- sapply(1:length(Parameters), function(x) {
    abs((EV_p_exceed_transient_OAT[50,x] - EV_p_exceed_transient_OAT[1,x]) / EV_p_exceed_transient_range[x])
  })
  
  total_costs_range_minus <- sapply(1:length(Parameters), function(x) {
    abs((total_costs_OAT[50,x] - total_costs_OAT[1,x]) / total_costs_range[x])
  })

# Above (quantile 99 - quantile 50)
  costs_range_plus <- sapply(1:length(Parameters), function(x) {
    abs((costs_OAT[99,x] - costs_OAT[50,x]) / costs_range[x])
  })
  
  NPV_expected_losses_range_plus <- sapply(1:length(Parameters), function(x) {
    abs((NPV_expected_losses_OAT[99,x] - NPV_expected_losses_OAT[50,x]) / NPV_expected_losses_range[x])
  })
  
  EV_p_exceed_transient_range_plus <- sapply(1:length(Parameters), function(x) {
    abs((EV_p_exceed_transient_OAT[99,x] - EV_p_exceed_transient_OAT[50,x]) / EV_p_exceed_transient_range[x])
  })
  
  total_costs_range_plus <- sapply(1:length(Parameters), function(x) {
    abs((total_costs_OAT[99,x] - total_costs_OAT[50,x]) / total_costs_range[x])
  })

# Normalize total range to 1 by dividing by largest range in each series
costs_range                   = costs_range/max(costs_range)
NPV_expected_losses_range     = NPV_expected_losses_range/max(NPV_expected_losses_range)
EV_p_exceed_transient_range   = EV_p_exceed_transient_range/max(EV_p_exceed_transient_range)
total_costs_range             = total_costs_range/max(total_costs_range)
  
# Normalize 50% +/- vectors to 1
# This makes each variance a fraction of the largest range, as well as showing what percentage
# of each range lies above and below the center line
for(i in 1:length(Parameters)){
  costs_range_plus[i] = costs_range_plus[i] * costs_range[i]
  costs_range_minus[i] = costs_range_minus[i] * costs_range[i]
  
  NPV_expected_losses_range_plus[i] = NPV_expected_losses_range_plus[i] * NPV_expected_losses_range[i]
  NPV_expected_losses_range_minus[i] = NPV_expected_losses_range_minus[i] * NPV_expected_losses_range[i]
  
  EV_p_exceed_transient_range_plus[i] = EV_p_exceed_transient_range_plus[i] * EV_p_exceed_transient_range[i]
  EV_p_exceed_transient_range_minus[i] = EV_p_exceed_transient_range_minus[i] * EV_p_exceed_transient_range[i]
  
  total_costs_range_plus[i] = total_costs_range_plus[i] * total_costs_range[i]
  total_costs_range_minus[i] = total_costs_range_minus[i] * total_costs_range[i]
}

# Sort the full ranges in decreasing order
costs_range                   = sort(costs_range, decreasing = TRUE)
NPV_expected_losses_range     = sort(NPV_expected_losses_range, decreasing = TRUE)
EV_p_exceed_transient_range   = sort(EV_p_exceed_transient_range, decreasing = TRUE)
total_costs_range             = sort(total_costs_range, decreasing = TRUE)

# Sort the plus/minus vectors so they represent the same decreasing order as the full range
costs_range_plus = costs_range_plus[order(match(names(costs_range_plus), names(costs_range)))]
costs_range_minus = costs_range_minus[order(match(names(costs_range_minus), names(costs_range)))]

NPV_expected_losses_range_plus = NPV_expected_losses_range_plus[order(match(names(NPV_expected_losses_range_plus), names(NPV_expected_losses_range)))]
NPV_expected_losses_range_minus = NPV_expected_losses_range_minus[order(match(names(NPV_expected_losses_range_minus), names(NPV_expected_losses_range)))]

EV_p_exceed_transient_range_plus = EV_p_exceed_transient_range_plus[order(match(names(EV_p_exceed_transient_range_plus), names(EV_p_exceed_transient_range)))]
EV_p_exceed_transient_range_minus = EV_p_exceed_transient_range_minus[order(match(names(EV_p_exceed_transient_range_minus), names(EV_p_exceed_transient_range)))]

total_costs_range_plus = total_costs_range_plus[order(match(names(total_costs_range_plus), names(total_costs_range)))]
total_costs_range_minus = total_costs_range_minus[order(match(names(total_costs_range_minus), names(total_costs_range)))]

### Plot results
# Set up matrix for positioning polygons
start = matrix(0, nrow = length(Parameters), ncol = 2)
start[1,] = c(0,1)

space = 0.25  # Space between bars
width = 0.15  # Width of bars

for(i in 2:length(Parameters)){
  start[i,2] = start[i-1,2] - space
}

# Set up matrix for plotting text in figure margins
lab_text = start
lab_text[1:length(Parameters),1] = 1.15
for(i in 1:length(Parameters)){
  lab_text[i,2] = start[i,2]-(width/2)
}

# Set up color vectors for each objectives
cost_col          = c(OAT_col[5], "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray", "dark gray")
damages_col       = c(OAT_col[10], OAT_col[11], OAT_col[1], OAT_col[2], OAT_col[4], OAT_col[8], OAT_col[3], OAT_col[7], OAT_col[6], OAT_col[9], "dark gray")
reliability_col   = c(OAT_col[10], OAT_col[11], OAT_col[1], OAT_col[2], OAT_col[8], OAT_col[7], OAT_col[6], OAT_col[9], "dark gray", "dark gray", "dark gray")
total_cost_col    = c(OAT_col[10], OAT_col[5], OAT_col[11], OAT_col[1], OAT_col[2], OAT_col[4], OAT_col[8], OAT_col[3], OAT_col[7], OAT_col[6], OAT_col[9])

# Plot results
pdf("Figures/OAT_Tornado.pdf", width = 8, height = 8)
par(oma = c(0,0,0,0)+0.1, mar = c(5,5,2,1)+0.1, mfrow = c(2,2))

# Costs
plot(0, type = 'n',
     xlim = c(-1, 1.25), ylim = c(-2, 1),
     xaxt = 'n', yaxt = 'n',
     xaxs = 'i',
     xlab = "Percent of total variance", ylab = "")

  for(i in 1:length(Parameters)) {
    if(costs_range[i]>0){
      polygon(x = c((0-costs_range_minus[i]), (0+costs_range_plus[i]), (0+costs_range_plus[i]), (0-costs_range_minus[i])),
              y = c(start[i,2], start[i,2], (start[i,2] - width), (start[i,2]-width)), 
              border = "black",
              col = cost_col[i])
    }
  }
axis(side = 1, at = seq(-1,1, by = 0.2), labels = seq(-100, 100, by = 20))
text(x = lab_text[,1], y = lab_text[,2], labels = c(expression("k", "", "", "", "", "", "", "", "", "", "")))
abline(v = 0, lty = 2)
box(lwd = 1.5)
mtext("A. Investment Costs", font = 2, side = 3, line = 0.25, at = -1, adj = c(0,0))

# Damages
plot(0, type = 'n',
     xlim = c(-1, 1.25), ylim = c(-2, 1),
     xaxt = 'n', yaxt = 'n',
     xaxs = 'i',
     xlab = "Percent of total variance", ylab = "")

  for(i in 1:length(Parameters)) {
    if(NPV_expected_losses_range[i]>0){
      polygon(x = c((0-NPV_expected_losses_range_minus[i]), (0+NPV_expected_losses_range_plus[i]), (0+NPV_expected_losses_range_plus[i]), (0-NPV_expected_losses_range_minus[i])),
              y = c(start[i,2], start[i,2], (start[i,2] - width), (start[i,2]-width)), 
              border = "black",
              col = damages_col[i])
    }
  }
axis(side = 1, at = seq(-1,1, by = 0.2), labels = seq(-100, 100, by = 20))
text(x = lab_text[,1], y = lab_text[,2], labels = c("t*", "c*", bquote(p[0]), expression(alpha), delta~"'", "b", "V", "a", expression(eta), "c", ""))
abline(v = 0, lty = 2)
box(lwd = 1.5)
mtext("B. NPV: Damages", font = 2, side = 3, line = 0.25, at = -1, adj = c(0,0))

  # Reliability
plot(0, type = 'n',
     xlim = c(-1, 1.25), ylim = c(-2, 1),
     xaxt = 'n', yaxt = 'n',
     xaxs = 'i',
     xlab = "Percent of total variance", ylab = "")

  for(i in 1:length(Parameters)) {
    if(EV_p_exceed_transient_range[i]>0){
      polygon(x = c((0-EV_p_exceed_transient_range_minus[i]), (0+EV_p_exceed_transient_range_plus[i]), (0+EV_p_exceed_transient_range_plus[i]), (0-EV_p_exceed_transient_range_minus[i])),
              y = c(start[i,2], start[i,2], (start[i,2] - width), (start[i,2]-width)), 
              border = "black",
              col = reliability_col[i])
    }
  }
axis(side = 1, at = seq(-1,1, by = 0.2), labels = seq(-100, 100, by = 20))
text(x = lab_text[,1], y = lab_text[,2], labels = c("t*", "c*", bquote(p[0]), expression(alpha), "b", "a", expression(eta), "c", "", "", ""))
abline(v = 0, lty = 2)
box(lwd = 1.5)
mtext("C. Reliability", font = 2, side = 3, line = 0.25, at = -1, adj = c(0,0))

  # Total Costs
plot(0, type = 'n',
     xlim = c(-1, 1.25), ylim = c(-2, 1),
     xaxt = 'n', yaxt = 'n',
     xaxs = 'i',
     xlab = "Percent of total variance", ylab = "")

  for(i in 1:length(Parameters)) {
    if(total_costs_range[i]>0){
      polygon(x = c((0-total_costs_range_minus[i]), (0+total_costs_range_plus[i]), (0+total_costs_range_plus[i]), (0-total_costs_range_minus[i])),
              y = c(start[i,2], start[i,2], (start[i,2] - width), (start[i,2]-width)), 
              border = "black",
              col = total_cost_col[i])
    }
  }
axis(side = 1, at = seq(-1,1, by = 0.2), labels = seq(-100, 100, by = 20))
text(x = lab_text[,1], y = lab_text[,2], labels = c("t*", "k", "c*", bquote(p[0]), expression(alpha), delta~"'", "b", "V", "a", expression(eta), "c"))
abline(v = 0, lty = 2)
box(lwd = 1.5)
mtext("D. NPV: Total Costs", font = 2, side = 3, line = 0.25, at = -1, adj = c(0,0))

dev.off()
