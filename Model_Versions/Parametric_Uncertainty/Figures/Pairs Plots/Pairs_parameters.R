###################################
# file: Pairs_parameters.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Produces pairwise scattter plots for model Parameters
# Utilizes plotutils.R function written by:
# Nathan Urban (nurban@lanl.gov)
#################################### 

# Set working directory
#setwd("~/vanDantzig/Model_Versions/Parametric_Uncertainty/Figures/Pairs Plots")

# Pairs plot for parameters
source("../../Scripts/plotutils.R")

pdf("Pairs_parameters.pdf")
plot.pairs(Parameters, labels = c(bquote(p[0]), expression(alpha), "V", expression(delta ~ "'"), "k", expression(eta), expression(phi)))

dev.off()
