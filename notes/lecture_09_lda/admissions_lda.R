# Admissions data: LDA
# WJW 2014

# Get data
admiss <- read.table("T11-6.DAT", header=FALSE)
colnames(admiss) <- c("GPA", "GMAT", "y")
head(admiss)

# Plot training data
plot(GMAT ~ GPA, data = admiss, pch = c(3, 4, 21)[y], 
   cex = 1.25, col = c("green", "red", "blue")[y])

# LDA using variables GPA and GMAT
library(MASS)
admiss.lda <- lda(y ~ GPA + GMAT, data = admiss)
admiss.lda

# Compute estimated probabilities on a grid of GPA and GMAT
GPA.range <- range(admiss$GPA)
GMAT.range <- range(admiss$GMAT)
x <- expand.grid(GPA = seq(GPA.range[1], GPA.range[2], length = 101),
     GMAT = seq(GMAT.range[1], GMAT.range[2], length = 101))
z <- predict(admiss.lda, newdata = x)$posterior

# 3 posterior probabilities are estimated for each point on the grid.
# We want to plot the max, so maximize over each row of z.
z <- apply(z, 1, max)

# Plot data again and add contour plot of max probability
plot(GMAT ~ GPA, data = admiss, pch = c(3, 4, 21)[y], 
   cex = 1.25, col = c("green", "red", "blue")[y])
z <- matrix(z, nrow = 101, ncol = 101, byrow = F)
# Contour plot of max estimated probability
contour(x = unique(x[, 1]), y = unique(x[, 2]), z, 
   levels = c(0.6, 0.9), lty = 2, add = T)




