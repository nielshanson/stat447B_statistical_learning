# Vaso data: plot fitted LDA model
# WJW 2014

#pdf('vaso-lda-plot-2d.pdf',  bg = 'transparent')

# Get data
data(vaso, package = 'robustbase')
head(vaso)

# LDA using Volume and Rate
library(MASS)
vaso.lda <- lda(Y ~ Volume + Rate, data = vaso, prior = c(0.5, 0.5))

# Plot training data
plot(Rate ~ Volume, data = vaso,  
   pch = c("O", "+")[Y + 1], cex = 1.25, col = c("red", "blue")[Y + 1])

# Plot estimated probabilities
Volume.range <- range(vaso$Volume)
Rate.range <- range(vaso$Rate)
x <- expand.grid(Volume = seq(Volume.range[1], Volume.range[2], length = 21),
     Rate = seq(Rate.range[1], Rate.range[2], length = 21))
z <- predict(vaso.lda, newdata = x)$posterior[, "1"]
z <- matrix(z, nrow = 21, ncol = 21, byrow = F)
# Contour plot of estimated probabilities
contour(x = unique(x[, 1]), y = unique(x[, 2]), z, levels = c(0.25, 0.5, 0.75), 
     lty = 2, add = T)




