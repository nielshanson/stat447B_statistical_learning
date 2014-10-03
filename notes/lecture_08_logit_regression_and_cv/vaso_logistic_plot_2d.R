# Vaso data: plot fitted logistic regression (glm)
# WJW 2014

#pdf('vaso-glm-plot-2d.pdf',  bg = 'transparent')

# Get data
data(vaso, package = 'robustbase')
head(vaso)

# Logistic regression on Volume and Rate
vaso.glm <- glm(Y ~ Volume + Rate, data = vaso, family = 'binomial')
summary(vaso.glm)

# Plot training data
plot(Rate ~ Volume, data = vaso,  
   pch = c("O", "+")[Y + 1], cex = 1.25, col = c("red", "blue")[Y + 1])

# Plot estimated probabilities
Volume.range <- range(vaso$Volume)
Rate.range <- range(vaso$Rate)
x <- expand.grid(Volume = seq(Volume.range[1], Volume.range[2], length = 21),
     Rate = seq(Rate.range[1], Rate.range[2], length = 21))
z <- predict(vaso.glm, newdata = x, type = "response")
z <- matrix(z, nrow = 21, ncol = 21, byrow = F)
# Contour plot of estimated probabilities
contour(x = unique(x[, 1]), y = unique(x[, 2]), z, levels = c(0.25, 0.5, 0.75), 
     lty = 2, add = T)




