#load package for parallel coordinate plot
library(MASS)
library(fields)
library(sfsmisc)

load(file = "../../../../../vanDantzig_largefiles/Model_Versions/Uncertainty_SLR_GEV/SLR_GEV.RData")

#read data 
M = Objectives
min.vals <- sapply(1:ncol(M), function(x) 
  {
  signif(min(M[,x]), digits = 3)
  })

max.vals <- sapply(1:ncol(M), function(x) 
  {
  signif(max(M[,x]), digits = 3)
  })

min.vals <- signif(min.vals, digits = 3)
# define "brushing" thresholds
# useful to focus on a few solutions instead of all at once
# thresholds row format: column number, threshold value, less than (-1) or greater than (1)
# not all columns need to have thresholds applied

thresholds <- matrix(c(4, (1/100),-1), nrow = 1, ncol = 3, byrow = TRUE)

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
# brush_off_idx contains the remaining indices that do not satisfy these.


#make color_map.  This one scales based on column 1 values
col_to_plot <- M[brush_on_idx,4]

ColorRamp <- c(colorRampPalette(c("blue", "light blue"))(50),
               colorRampPalette(c("light blue", "light green", "yellow"))(55),
               colorRampPalette(c("yellow", "orange", "red", "darkred"))(70))

ColorRampAlpha <- apply(sapply(ColorRamp, col2rgb)/255, 2, 
                        function(x) rgb(x[1], x[2], x[3], alpha=0.1))

ColorLevels <- seq(min(col_to_plot), 1, length=length(ColorRamp))

z_scl <- (col_to_plot - min(col_to_plot, na.rm=T))/
  (max(col_to_plot, na.rm=T) - min(col_to_plot, na.rm=T))
color_scl = round(z_scl*length(ColorRamp))
color_scl[color_scl == 0] = 1

#concatenate brushed off and brushed on indices in a matrix for plotting
all_solutions =rbind(M[brush_off_idx, ],M[brush_on_idx, ]) 

colnames(all_solutions) <- c("Total Costs", "Costs", "Expected Damages", "Flood Probability")

# Set layout to include a colorbar next to plot.
# Parallel coord function:
parcoord2 <- function (x, col = 1, lty = 1, var.label = FALSE, ...) 
{
  rx <- apply(x, 2L, range, na.rm = TRUE)
  x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/(max(x, 
                                                                na.rm = TRUE) - min(x, na.rm = TRUE)))
  
  matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty, lwd = 0.65, 
          xlab = "", ylab = "", axes = FALSE, ...)
  axis(1, at = 1L:ncol(x), labels = colnames(x), lty = 0)
  axis(1, at = 1L:ncol(x), labels = pretty10exp(min.vals), lty = 0, line = -1, cex.axis = 0.65, col.axis = "gray48")
  axis(3, at = 1L:ncol(x), labels = pretty10exp(max.vals), lty = 0, line = -1, cex.axis = 0.65, col.axis = "gray48")
  
  abline(v=1:ncol(x), lty=1, lwd=1.5)
  for (i in 1L:ncol(x)) {
    lines(c(i, i), c(0, 1), col = "black")
    if (var.label) 
      text(c(i, i), c(0, 1), labels = format(rx[, i], digits = 3), 
           xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
  }
  invisible()
}

#plot solutions
png("parallel_axis.brush.png", width = 6.5, height = 4.5, unit = 'in', res  = 300)
par(mar = c(8,2,1,2), oma = c(0,0.5,1,0.5), font = 1)
parcoord2(all_solutions, 
         col=c(rep.int("grey",length(brush_off_idx)),ColorRampAlpha[color_scl]), 
         var.label=FALSE,
         xaxt = "n")
arrows(x0=0.91, y0=0.95, y1=0.05, length=0.1, lwd = 2)
mtext("Preference", side = 2, line = 1, cex = par("cex.lab"))

# Add colorbar
image.plot(1, ColorLevels, add = T, legend.line = 6, smallplot =  c(0.15,0.85,0.06,0.1),
      matrix(data=ColorLevels, ncol=length(ColorRamp),nrow=1), legend.only = T, 
      col=ColorRamp,xlab="",ylab="Column 1 Value",xaxt="n", las = 1, axes = F, horizontal = T,
      legend.shrink = 0.75, legend.width = 0.85, 
      axis.args = list(at = c(0,1), labels = c(pretty10exp(min.vals)[4], pretty10exp(1e-02)), tick = F, line = -0.85, cex.axis = 0.85),
      legend.args = list(side = 3, line = 0.35, text = "Flood Probability for brushed set (1/yr)", cex = 0.85))

dev.off()
### End of Plotting