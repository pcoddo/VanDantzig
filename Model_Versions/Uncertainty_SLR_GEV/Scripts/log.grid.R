###################################
# file: log.grid.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Adds logarithmic grid to log plots
#################################### 

log.grid <- function(limit, axis, ...){
  
  require(magicaxis)
  
  end = log10(limit)
  sign = sign(end)
  sequence = seq(0, end, by = sign)

  if(axis == 1 | axis == "x")
    {
    abline(h = axTicks(side = 2), ...)
    for(i in 1:(abs(end)+1) )
      {
      abline(v = c(seq( 1*10^sequence[i], 9*10^sequence[i], 1*10^sequence[i])),...)
      }
    }
  
  else 
    {
    abline(v = axTicks(side = 1), ...)
    for(i in 1:(abs(end)+1) )
      {
      abline(h = c(seq( 1*10^sequence[i], 9*10^sequence[i], 1*10^sequence[i])),...)
      }
    }
}