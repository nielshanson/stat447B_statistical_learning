# Lidar data: polynomial regression
# Adapted from Matias' R code

# Get data
library(SemiPar)
data(lidar, package = 'SemiPar')

# x-y plot
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)

# Degree 4 polynomial model
# I(range^2) ensures range^2 is interpreted as squared range in formula, etc.
pm <- lm(logratio ~ range + I(range^2) + I(range^3) + I(range^4), data=lidar)
rr <- seq(390, 720, length = 1000) 
lines(rr, predict(pm, newdata = list(range = rr)), lwd = 4, col = 'blue')

# Degree 10 polynomial model
pm2 <- lm(logratio ~ range + I(range^2) + I(range^3) + I(range^4) 
   + I(range^5) + I(range^6) + I(range^7) + I(range^8) + I(range^9) + I(range^10),
   data = lidar)
lines(rr, predict(pm2, newdata = list(range = rr)), lwd = 4, col = 'red')

# Warning is given that X matrix may be rank-deficient!