# Vaso data: logistic regression (glm)
# WJW 2014

# Get data
data(vaso, package = 'robustbase')
head(vaso)

#pdf('vaso-glm-volume.pdf',  bg = 'transparent')

# Logistic regression on Volume
plot(Y ~ Volume, data = vaso, pch = 19, col = Y + 1, cex = 1.5)
vaso.glm <- glm(Y ~ Volume, data = vaso, family = 'binomial')
summary(vaso.glm)
lines(sort(vaso$Volume), vaso.glm$fitted.values[order(vaso$Volume)], 
   lwd = 3, cex = 1.25)

# Misclassification matrix - training data
vaso.pred <- predict(vaso.glm, type = "response")
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
   vaso.glm <- glm(Y ~ Volume, data = vaso[fold != i, ], family = 'binomial')

   # p hat predictions for test data
   pred.cv.this.fold <- predict(vaso.glm, newdata = vaso[fold == i, ], 
      type = "response")

   # Put test p hats in the right positions in pred.cv
   pred.cv[fold == i] <- pred.cv.this.fold
}

# Misclassification matrix - cross-validation
print(table(vaso$Y, pred.cv > 0.5))

# Logistic regression on Volume and Rate
vaso.glm <- glm(Y ~ Volume + Rate, data = vaso, family = 'binomial')
summary(vaso.glm)
vaso.pred <- predict(vaso.glm, type = "response")

# Misclassification matrix - training data
print(table(vaso$Y, vaso.pred > 0.5))

# v-fold cross-validation with v = 10 (same folds as for other model)

# For each fold
for (i in 1:v)
{
   # Fit using the training data only 
   vaso.glm <- glm(Y ~ Volume + Rate, data = vaso[fold != i, ], family = 'binomial')


   # p hat predictions for test data
   pred.cv.this.fold <- predict(vaso.glm, newdata = vaso[fold == i, ], 
      type = "response")

   # Put test p hats in the right positions in pred.cv
   pred.cv[fold == i] <- pred.cv.this.fold
}

# Misclassification matrix - cross-validation
print(table(vaso$Y, pred.cv > 0.5))

