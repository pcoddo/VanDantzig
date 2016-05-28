error.bars <- function(x, y, upper, lower=upper, length=0.1, horiz=F,...) {
	if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
	stop("vectors must be same length")
	if (horiz) {
		arrows(upper, y, lower, y, angle=90, code=3, length=length, ...)
		}
	else {
		arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
		}
		
	}
