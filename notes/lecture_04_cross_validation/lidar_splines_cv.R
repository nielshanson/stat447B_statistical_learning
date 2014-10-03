# Lidar data: cross-validation applied to splines
# WJW 2014

# Get data
library(SemiPar)
data(lidar, package = 'SemiPar')

# x-y plot
plot(logratio ~ range, data = lidar, pch = 19, col = 'gray', cex = 1.5)

# Make the work easier by creating a function 
# to generate spline basis functions from a vector x.
# Copied from Lecture 3 R code with no changes.

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

# Set up folds for v-fold cross-validation with v = 2
v <- 2
# So the (random) grouping can be reproduced.
# You should use a DIFFERENT seed.
set.seed(100) 
fold = sample(rep(1:v, length=nrow(lidar)))
# Look at first few values of fold
fold[1:10]
# Number of observations in each fold (group)
sum(fold == 1)
sum(fold == 2)

# Set up linear spline with 5 knots for *all* data, training and test
x.mat <- spline.basis(lidar[, "range"], nknots = 5, knots = NULL,  degree = 1)
colnames(x.mat) <- c("range", "range.knot1", "range.knot2", "range.knot3", 
   "range.knot4", "range.knot5")
# Expand the lidar dataframe to include the new columns
lidar <- data.frame(logratio = lidar$logratio, x.mat)

# Fit the regression model using the training data (fold == 1) only
spline.fit <- lm(logratio ~ range + range.knot1 + range.knot2 + range.knot3 + range.knot4 + range.knot5, 
   data = lidar[fold == 1, ])
summary(spline.fit)
# Predict the observations in fold 2
logratio.2 <- predict(spline.fit, newdata = lidar[fold == 2, ]) 
# Add the predictions to lidar
lidar <- data.frame(lidar, logratio.cv = rep(0, length = nrow(lidar)))
lidar[fold == 2, "logratio.cv"] <- logratio.2
# Check this makes sense by plotting
# Note fewer points plotted - only the test data
plot(logratio ~ range, data = lidar[fold == 2, ], 
   pch = 19, col = 'gray', cex = 1.5)
# Add the predictions for the test data
lines(logratio.cv ~ range, data = lidar[fold == 2, ], lwd=4, col = 'green')
# RMSE
err <- lidar$logratio[fold == 2] - lidar$logratio.cv[fold == 2]
rmse <- sqrt(sum(err^2) / length(err))
print(rmse)

# The above can be repeated with fold 2 for training and fold 1 for test.
# But in general it's easier to do everything (including the above) in a loop

# A LOT OF WORK if we want to try different models.
# Let's automate this with a function.

lm.cv.rmse <- function(x.mat, y, v = 10, verbose = TRUE)
# Return the v-fold cross-validation predictions for regression 
# fits of y on x.mat via lm
{ 
   # Create a dataframe for x.mat and y 
   xy <- data.frame(y, x.mat)
   # Vector to hold the cv predictions
   pred.cv <- rep(0, length = nrow(xy))

   # Set up v folds
   set.seed(100) 
   fold = sample(rep(1:v, length=nrow(xy)))

   for (i in 1:v)
   {
      # Fold i will be test data; training data exclude fold i
 
      # The dot in the model formula says use all other variables 
      lm.out <- lm(y ~ ., data = xy[fold != i, ])
      if (verbose)
         print(summary(lm.out))

      # Predict the observations in the test data
      pred.cv.this.fold <- predict(lm.out, newdata = xy[fold == i, ]) 
      # Put the cv predictions in the correct positions in pred.cv
      pred.cv[fold == i] <- pred.cv.this.fold
   }

   return(pred.cv)

}

library(SemiPar)
data(lidar, package = 'SemiPar')

# 5 knots, v = 2
x.mat <- spline.basis(lidar$range, nknots = 5,  degree = 1)
pred.cv <- lm.cv.rmse(x.mat, lidar$logratio, v = 2)
err <- lidar$logratio - pred.cv
rmse <- sqrt(sum(err^2) / length(err))
print(rmse)

# 5 knots, v = 10
x.mat <- spline.basis(lidar$range, nknots = 5,  degree = 1)
pred.cv <- lm.cv.rmse(x.mat, lidar$logratio, v = 10)
err <- lidar$logratio - pred.cv
rmse <- sqrt(sum(err^2) / length(err))
print(rmse)

# 10 knots, v = 10
x.mat <- spline.basis(lidar$range, nknots = 10,  degree = 1)
pred.cv <- lm.cv.rmse(x.mat, lidar$logratio, v = 10, verbose = FALSE)
err <- lidar$logratio - pred.cv
rmse <- sqrt(sum(err^2) / length(err))
print(rmse)

library(SemiPar)
data(lidar, package = 'SemiPar')

# For 2 plots on a page (quadratic and cubic splines)
par(mfrow = c(1, 2))

# Quadratic splines, choose number of knots via CV
nknots = seq(1:10)
rmse <- rep(0, length = length(nknots))
for (k in 1:length(nknots))
{
   x.mat <- spline.basis(lidar$range, nknots = nknots[k],  degree = 2)
   pred.cv <- lm.cv.rmse(x.mat, lidar$logratio, v = 10, verbose = FALSE)
   err <- lidar$logratio - pred.cv
   rmse[k] <- sqrt(sum(err^2) / length(err))
}
print(rmse)
plot(nknots, rmse, ylim = c(0.08, 0.10), xlab = "Number of knots", ylab = "CV RMSE", pch = 19, cex = 1.5)

# Cubic splines, choose number of knots via CV
nknots = seq(1:10)
rmse <- rep(0, length = length(nknots))
for (k in 1:length(nknots))
{
   x.mat <- spline.basis(lidar$range, nknots = nknots[k],  degree = 3)
   pred.cv <- lm.cv.rmse(x.mat, lidar$logratio, v = 10, verbose = TRUE)
   err <- lidar$logratio - pred.cv
   rmse[k] <- sqrt(sum(err^2) / length(err))
}
print(rmse)
plot(nknots, rmse, ylim = c(0.08, 0.10), xlab = "Number of knots", ylab = "CV RMSE", pch = 19, cex = 1.5)

