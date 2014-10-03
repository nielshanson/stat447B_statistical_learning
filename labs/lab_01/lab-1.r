d <- read.table("D:/ustemp.txt", header=T)
y <- d$min.temp;
x1 <- d$latitude;
x2 <- d$longitude;

# Scatterplots
plot(x1,y);
plot(x2,y);

# Polynomial regression
# X <- cbind(x1,x2,x2^2,x2^3); #Can create the design matrix like this
poly.fit <- lm(y~x1+x2+I(x2^2)+I(x2^3));
summary(poly.fit);
poly.conf <- predict(poly.fit, interval="confidence"); #CI

# Cubic splines
# 1. choose the number of knots (set as 3)
# 2. place the knots at evenly spaced positions
# 3. build the design matrix
# 3a. if necessary, build the design matrix again using b-splines
# 4. fit the model

# Prepare the matrix
x2 <- x2+90; #This reduces the order of magnitude of the numbers, making the resulting
			 #design matrix computationally more stable. Needed if you want to find the
			 #estimates yourself by inverting X'X.
K <- 3;
kn <- as.numeric(quantile(x2,(1:K)/(K+1)));
XX <- x1;
for (j in 1:3){
	XX <- cbind(XX,x2^j);
}
for (j in 1:length(kn)){
	XX <- cbind(XX,pmax(x2-kn[j],0)^3);
}
# You can also try to create this matrix using the self-defined function used in lecture

# Fit the model
cubic.splines.fit <- lm(y~XX);
summary(cubic.splines.fit);
splines.conf <- predict(cubic.splines.fit,interval="confidence");
		
# Sum of squared errors
cbind(y,poly.conf[,1],splines.conf[,1]); #"Visualize" predictions
sum((y-poly.conf[,1])^2);
sum((y-splines.conf[,1])^2);

# Confidence intervals
plot(y,pch=21,bg=rgb(0.5,0.5,1));
for(i in 1:length(x1)){
	points(i,poly.conf[i,1],pch=21,col='red');
	lines(c(i,i),poly.conf[i,2:3],col='red');
	lines(c(i-0.2,i+0.2),rep(poly.conf[i,2],2),col='red');
	lines(c(i-0.2,i+0.2),rep(poly.conf[i,3],2),col='red');
	
	points(i,splines.conf[i,1],pch=21,col=rgb(0,0.7,0,0.5));
	lines(c(i,i),splines.conf[i,2:3],col=rgb(0,0.7,0,0.5));
	lines(c(i-0.2,i+0.2),rep(splines.conf[i,2],2),col=rgb(0,0.7,0,0.5));
	lines(c(i-0.2,i+0.2),rep(splines.conf[i,3],2),col=rgb(0,0.7,0,0.5));
}

# Another way of visualizing the results
x2 <- d$longitude; #Use the original longitudes

beta0 <- poly.fit$coefficients[1];
beta1 <- poly.fit$coefficients[2];
beta2 <- poly.fit$coefficients[3];
beta3 <- poly.fit$coefficients[4];
beta4 <- poly.fit$coefficients[5];

x1bar <- mean(x1);
long <- seq(min(x2),max(x2),0.1);
min.temp.pred <- beta0+beta1*x1bar+beta2*long+beta3*long^2+beta4*long^3;

plot(x2,y);
lines(long,min.temp.pred);

# Plot in 3d
library(rgl);
latgrid <- seq(min(x1),max(x1),1);
longgrid <- seq(min(x2),max(x2),1);
mat <- cbind(expand.grid(longgrid,latgrid),0);
mat[,3] <- beta0+beta1*mat[,2]+beta2*mat[,1]+beta3*mat[,1]^2+beta4*mat[,1]^3;
persp3d(longgrid,latgrid,mat[,3],col='pink');
points3d(cbind(x2,x1,y),col="red");

# You can modify the code to display the spline fit instead