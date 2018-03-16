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
setwd("~/vanDantzig/Model_Versions/Parametric_Uncertainty/Figures/Pairs Plots")

# Pairs plot for parameters
source("../../SLR_Module/Rejection_Sampling/plotutils.R")

pdf("Pairs_parameters.pdf")
pairs <- data.frame(Parameters[3:6], beta.dist[1:5])

#plot.pairs(pairs, labels = c("V", expression(delta ~ "'"), "k", expression(eta), "a", "b", "c", "t*", "c*"))
plot.pairs(beta.dist[1:5], labels = c("a", "b", "c", "t*", "c*"))
dev.off()
