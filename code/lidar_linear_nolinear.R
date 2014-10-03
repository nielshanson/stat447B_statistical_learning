# Lidar data: linear versus nonlinear regression models
# Adapted from Matias' R code

# Get data
library(SemiPar)
data(lidar, package = 'SemiPar')

# x-y plot
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)

# Fit and show a simple linear regression model
ml <- lm(logratio ~ range, data = lidar)
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)
abline(ml, lwd=4,  col = 'blue')

# Sinusoidal relationship, but still a linear model
ml2 <- lm(logratio ~ range + sin((range - 400) * pi / 300), data = lidar)

# Add the fitted curve by a dense set of predicted values
# grid of range values at which to plot
rr <- seq(390, 720, length = 1000) 
# predictions of logratio at rr values
pr <- predict(ml2, newdata = list(range = rr)) 
# Plot predictions, joining them by lines
lines(rr, pr, lwd=4,  col = 'green')

# A nonlinear model
mnl1 <- nls(logratio ~ a + b * range
   + d * range * sin((range - 400) * pi / 300) * exp(e * (range - 400) / 500)
   + h * range * cos((range - 400) * pi / 300) * exp(j * (range - 400) / 500),  
   data = lidar, start = list(a = 1, b = 1, d = 1, e = 1, h = 1, j = 1))
# New plot of data, with fitted curve
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)
lines(rr, predict(mnl1, newdata = list(range = rr)), lwd = 4, col = 'magenta')


