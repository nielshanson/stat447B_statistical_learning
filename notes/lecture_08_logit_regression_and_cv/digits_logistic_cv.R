# Get data
# You may need to include a path to the folder container the data 
x <- read.table("mfeatpix.dat", header = FALSE, 
   col.names = paste("p", 1:240, sep = ""))

# Extract "8" and "9" data (original data only has x's)
x89 <- x[1601:2000, ]

# Class codes
y <- c(rep(0, times = 200), rep(1, times = 200))

# Put x's and y together in a matrix
dig <- cbind(x89, y)

# Check data
head(dig)
tail(dig)

# Fit logistic regression model.
dig.logistic <- glm(y ~ ., family = "binomial", data = dig)
print(summary(dig.logistic))

# Misclassification table
print(table(dig[, "y"], dig.logistic$fitted.values > 0.5))

# 10-fold cross-validation

# Create vector to hold the cross-validated test-data p hats.
p.hat <- vector("numeric", length = nrow(dig))

# Divide the observation indices (1,..,n) into 10 folds (groups) at random.
set.seed(100)  
fold <- sample(rep(1:10,length=nrow(dig)))

# For each fold.
for (j in 1:10)
{
   test.data <- (fold == j)      # Observation indices in fold j.
   training.data <- (fold != j)  # Obs indices *not* in fold j.

   # Fit using the training data only (9/10 of the observations).
   dig.logistic <- glm(y ~ ., family = "binomial", 
      data = dig[training.data, ])

   # Estimate prob(Y=1) for test-data observations.
   p.hat.j <- predict(dig.logistic, newdata = dig[test.data, ], 
      type = "response")
   # Put test p hats in the right place in the vector of *all* p hats.
   p.hat[test.data] <- p.hat.j
}

# Misclassification table based on 10-fold cross-validation
print(table(dig[, "y"], p.hat > 0.5))

