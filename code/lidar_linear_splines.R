# Lidar data: linear splines
# Adapted from Matias' R code

# Get data
library(SemiPar)
data(lidar, package = 'SemiPar')

# x-y plot
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)

# linear splines with 5 knots
# select the knots at 5 quantiles
kn <- as.numeric( quantile(lidar$range, (1:5) / 6) )

# Create the X matrix of covariates / explanatory variables
x <- matrix(0, nrow = nrow(lidar), ncol = length(kn) + 1)
for(k in 1:length(kn)) 
   x[, k] <- pmax(lidar$range - kn[k], 0)
x[, length(kn)+1] <- lidar$range

# Fit the regression model
ppm <- lm(lidar$logratio ~ x)
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)
# Because the X matrix was created manually:
# - plot the predictions from the training data
# - this avoids creating another X matrix at a grid of new values of range
lines(predict(ppm)[order(range)] ~ sort(range), data = lidar, lwd=4, col = 'green')


