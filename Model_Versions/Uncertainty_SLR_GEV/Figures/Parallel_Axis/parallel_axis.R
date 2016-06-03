#load package for parallel coordinate plot
library(MASS)

load(file = "../../../../../vanDantzig_largefiles/Model_Versions/Uncertainty_SLR_GEV/SLR_GEV.RData")
#read data 
M=Objectives

# define "brushing" thresholds
# useful to focus on a few solutions instead of all at once
# thresholds row format: column number, threshold value, less than (-1) or greater than (1)
# not all columns need to have thresholds applied

thresholds <- matrix(c(2,0.2,1, 4,0.5,-1), nrow = 2, ncol = 3, byrow = TRUE)

#determine solutions to be brushed
IX <- seq(1,dim(M)[1])


for (i in 1:nrow(thresholds))
{
  col <- thresholds[i ,1]
  if(thresholds[i,3]==-1){
    tempIX <- rfind <- seq(along=M[ ,col])[as.logical(M[ ,col] < thresholds[i,2])]
  }
  else{
    tempIX <- rfind<- seq(along=M[ ,col])[as.logical(M[ ,col] > thresholds[i,2])]
  }
  
  IX <- intersect(IX,tempIX)
  
}

brush_on_idx <- unique(IX)
brush_off_idx <- setdiff(seq(1,nrow(M)),brush_on_idx)


#re-map the values in M to [0, 1] for consistent plotting
ncols = ncol(M)
for (i in 1:ncols){
  M[ ,i] <- (M[ ,i] - min(M[ ,i], na.rm=T))/(max(M[ ,i], na.rm=T) - min(M[ ,i], na.rm=T))
}

# brush_on_idx now contains the indices of the rows of M that satisfy:
# Column 2 greater than 0.2 (scaled value)
# Column 4 less than 0.5 (scaled value)
# brush_off_idx contains the remaining indices that do not satisfy these.


#make color_map.  This one scales based on column 1 values
col_to_plot <- M[brush_on_idx,1]

ColorRamp <- c(colorRampPalette(c("blue", "light blue"))(50),
               colorRampPalette(c("light blue", "light green", "yellow"))(55),
               colorRampPalette(c("orange", "red", "darkred"))(70))
#ColorLevels <- seq(min(col_to_plot), max(col_to_plot), length=length(ColorRamp))
ColorLevels <- seq(min(col_to_plot), 1, length=length(ColorRamp))

z_scl <- (col_to_plot - min(col_to_plot, na.rm=T))/
  (max(col_to_plot, na.rm=T) - min(col_to_plot, na.rm=T))
color_scl = round(z_scl*length(ColorRamp))
color_scl[color_scl == 0] = 1

#concatenate brushed off and brushed on indices in a matrix for plotting
all_solutions =rbind(M[brush_off_idx, ],M[brush_on_idx, ]) 

colnames(all_solutions) <- c("Total Costs", "Costs", "Expected Damages", "Flood Probability")

# Set layout to include a colorbar next to plot.
layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1),
       heights=c(1,1))
#plotting margins.  


#plot solutions
png("parallel_axis.png", width = 6.5, height = 4.5, unit = 'in', res  = 300)
layout(mat = matrix(c(1,2), 1, 2), widths = c(6, 0.85))
par(mar = c(5,2,1,1), oma = c(0,0,0,0)+0.5, font = 1.5)
parcoord(all_solutions, 
         col=c(rep.int("grey",length(brush_off_idx)),ColorRamp[color_scl]), 
         var.label=FALSE,
         xaxt = "n")

mtext("Scaled Values", side = 2, line = 1, cex = par("cex.lab"))
# Add a colorbar
#par(mar = c(5,3,2.5,3))
#par(mar = c(5.5,0,4.5,3))
par(mar = c(5.5,1,1.5,2))

image(1, ColorLevels,
      matrix(data=ColorLevels, ncol=length(ColorRamp),nrow=1),
      col=ColorRamp,xlab="",ylab="Column 1 Value",xaxt="n", las = 1, axes = F)
axis(side = 4, at = c(0,1), labels = TRUE, las = 1)
mtext("Column 1 Value", side = 4, line = 0.5, cex.lab = 0.5)

dev.off()
### End of Plotting