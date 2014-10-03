# Admissions data: logistic regression (3 classes)
# WJW 2014

# Get data
admiss <- read.table("T11-6.DAT", header=FALSE)
colnames(admiss) <- c("GPA", "GMAT", "y")
head(admiss)

# Logistic regression using multinom in nnet
library(nnet)
admiss.logistic <- multinom(y ~ GPA + GMAT, data = admiss, maxit = 10000)
print(summary(admiss.logistic))

# Compute estimated probabilities on a grid of GPA and GMAT
GPA.range <- range(admiss$GPA)
GMAT.range <- range(admiss$GMAT)
x <- expand.grid(GPA = seq(GPA.range[1], GPA.range[2], length = 101),
     GMAT = seq(GMAT.range[1], GMAT.range[2], length = 101))
z <- predict(admiss.logistic, newdata = x)

# Plot data again and add contour plot of class boundaries
plot(GMAT ~ GPA, data = admiss, pch = c(3, 4, 21)[y], 
   cex = 1.25, col = c("green", "red", "blue")[y])
z <- matrix(z, nrow = 101, ncol = 101, byrow = F)
# Contour plot of *class labels* gives the class boundaries
contour(x = unique(x[, 1]), y = unique(x[, 2]), z, 
   levels = c(1, 2, 3), lty = 2, add = T)




