# Vaso data: logistic regression (gam)
# WJW 2014

# Get data
data(vaso, package = 'robustbase')
head(vaso)

# GAM logistic regression on Volume and Rate using gam in library mgcv
library(mgcv)
vaso.gam <- gam(Y ~ s(Volume) + s(Rate), data = vaso, family = 'binomial')
summary(vaso.gam)
vaso.pred <- predict(vaso.gam, type = "response")

# Misclassification matrix - training data
print(table(vaso$Y, vaso.pred > 0.5))

# v-fold cross-validation with v = 10
v <- 10
# So the (random) grouping can be reproduced.
# You should use a DIFFERENT seed.
set.seed(100) 
# Divide the observation indices (1,..,n) into 10 folds (groups) at random
fold = sample(rep(1:v, length = nrow(vaso)))
# Look at first few values of fold
fold[1:10]
# Number of observations in first fold (group)
sum(fold == 1)

# Vector to hold the cv p hats
pred.cv <- rep(0, length = nrow(vaso))

# For each fold
for (i in 1:v)
{
   # Fit using the training data only 
   vaso.gam <- gam(Y ~ s(Volume) + s(Rate), data = vaso[fold != i, ], family = 'binomial')

   # p hat predictions for test data
   pred.cv.this.fold <- predict(vaso.gam, newdata = vaso[fold == i, ], 
      type = "response")

   # Put test p hats in the right positions in pred.cv
   pred.cv[fold == i] <- pred.cv.this.fold
}

# Misclassification matrix - cross-validation
print(table(vaso$Y, pred.cv > 0.5))

