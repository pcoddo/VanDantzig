###################################
## file - max_monthly_sl.R
###################################
## Perry Oddo
## Pennsylvania State University
## poddo@psu.edu
## 
## Data found at Permanent Service for Mean Sea Level (PSMSL)
## "Tide Gauge Data", Retrieved 08 Sep 2014 from http://www.psmsl.org/data/obtaining/
## Gauge Station #22 - Hook van Holland, Netherlands
###################################
## Last changed: 16 September, 2014
###################################
#
# Import Sea-level data - Maximum Monthly SL from Hook van Holland tide gauge:
sl_data <- read.table("max_monthly_sl.txt",header = TRUE, sep = "\t")
plot(sl_data$year, (sl_data$max_monthly_sl)/1000, type = "l", xlab = "Year", ylab = "Maximum Monthly Sea Level (m)", xlim = c(1860,2015))

# Optimize function to find initial parameters for storm surge
# Probability of exceedence formula: p_exceed[i]= p_zero_p*exp(-alpha_p*X[i])
est_anom = a*exp(-b*)

optim_sealevel = sl_data[,2]
optim_year = sl_data[,1]
optim_fn = function(x, optim_sealevel, optim_year) {
  a = x[1]
  b = x[2]
  c = x[3]
  offset = x[4]
  optim_rmse = sqrt(mean((data$sealevel - (a*(data$year - offset)^2 + b*(data$year - offset) + c))^2))
}

optim_fn
# PDF values from van Dantzig (1956)
# Section 6 - the Doubtful Constants:

# Flood frequency in 1/a at zero height increase
p_zero_p=0.0038

# Rate of exponential flood frequency decrease per m height increase
alpha_p=2.6


# Probability of exceeding a given sea level height
X=seq(0,10,by=0.05)
p_exceed = array(NA, length(sl_data$year))
for(i in 1:length(sl_data$year)) {
  p_exceed[i] = p_zero_p * exp(-alpha_p * X[i])
  
}


for (i in 1:length(sl_data$year) {
  p_exceed[i] = p_zero_p*exp(-alpha_p*X[i])
}
 