# Lidar data: linear splines
# WJW 2014
# Revised 2014.09.13 
###
attach("myFiles/software/R/myCode/.RData")
setwd("myFiles/teaching/stat447_2014/examples/lidar")

# Get data
library(SemiPar)
data(lidar, package = 'SemiPar')

# x-y plot
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)

# Make the work easier by creating a function 
# to generate spline basis functions from a vector x

spline.basis <- function(x, nknots = 1, knots = NULL,  degree = 1)
# Generate the X matrix for linear, quadratic, … splines (continuous derivatives).
# If knots is given a vector of values, these values are used as the knots
# and nknots is ignored.
# If knots is null, then nknots is the number of knots, and the knots are 
# generated as the quantiles of x.
# degree is 1 for linear splines, 2 for quadratic splines, etc.  
# The implied model is continuous and has continuous derivatives up to degree - 1.   
# Returns the X matrix: polynomial terms in x followed by basis functions in x.
{
   if (is.null(knots)) 
      knots <- as.numeric( quantile(x, (1:nknots) / (nknots + 1)) )
   else
      nknots = length(knots)
   
   # Create the X matrix of covariates / explanatory variables
   x.mat <- matrix(0, nrow = length(x), ncol = degree + nknots)
   # Polynomial terms in the first columns
   for (j in 1:degree)
      x.mat[, j] <- x^j
   # Further columns for spline basis functions
   for(k in 1:nknots) 
      x.mat[, degree+k] <- (pmax(x - knots[k], 0))^degree

   return(x.mat)
}

# Linear splines with 5 knot and 10 knots (at quantiles)
# (redo earlier fit, now using spline.basis)
fig.standard("lidar_linear_spline5v10.ps", mfrow = c(1,1), file.format = "ps")

# 5 knots
x.mat <- spline.basis(lidar[, "range"], nknots = 5, knots = NULL,  degree = 1)
# Fit the regression model
sp5 <- lm(lidar$logratio ~ x.mat)
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)
# Because the X matrix was created manually:
# - plot the predictions from the training data
# - this avoids creating another X matrix at a grid of new values of range
lines(predict(sp5)[order(range)] ~ sort(range), data = lidar, lwd=4, col = 'green')
summary(sp5)

# Repeat with 10 knots
x.mat <- spline.basis(lidar[, "range"], nknots = 10, knots = NULL,  degree = 1)
sp10 <- lm(lidar$logratio ~ x.mat)
lines(predict(sp10)[order(range)] ~ sort(range), data = lidar, lwd=4, col = 'blue')
summary(sp10)


# Linear splines with 1 knot at 550
#fig.standard("lidar_linear_spline550.ps", mfrow = c(1,1), file.format = "ps")
x.mat <- spline.basis(lidar[, "range"], nknots = 0, knots = 550,  degree = 1)

# Fit the regression model
sp550 <- lm(lidar$logratio ~ x.mat)
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)
lines(predict(sp550)[order(range)] ~ sort(range), data = lidar, lwd=4, col = 'green')

