# Trade-union data: linear versus nonlinear regression models
# Adapted from Matias' R code

# Get data
library(SemiPar)
data(trade.union, package = 'SemiPar')

# x-y plot
plot(wage ~ age, data = trade.union, pch = 19, col = 'gray', cex = 1.5)

# Simple linear regression
m1 <- lm(wage ~ age, data = trade.union)
lines(predict(m1)[order(age)] ~ sort(age), data = trade.union, lwd = 4, col = 'red')

#  * Nonlinear model
m2 <- nls(wage ~a +b * exp(-(age - 20) / c), start=list(a=1, b=-.1, c=1), 
   control = nls.control(minFactor = 2^(-20), maxiter = 100), trace=TRUE, 
   data = trade.union)
lines(predict(m2)[order(age)] ~ sort(age), data = trade.union, lwd = 4, col = 'blue')

# Compare the fits
summary(m1)
summary(m2)

# How do we compare these fits?  Sum of squared residuals?
sum(resid(m1)^2)
sum(resid(m2)^2)



