library(MASS);
dat <- read.csv("D:/iris.txt",header=T);
x1 <- dat[,1]; x2 <- dat[,2]; y <- dat[,3]; #y=0: versicolor; y=1: virginica
# Plot the data
plot(x1,x2,pch=19,col=y+1,cex=1.3,xlab="Sepal",ylab="Petal",xlim=c(3,8),ylim=c(3,8));


# Fit logistic regression model
glmobj <- glm(y~x1+x2,family="binomial");
# Predict at a grid of values spanning the range of covariates
xx1 <- seq(1,9,length=200);
xx2 <- seq(1,9,length=200);
dd <- expand.grid(x1=xx1,x2=xx2);
#colnames(dd) <- c('x1','x2'); #This is the line you need if you have to change the column names
yy <- predict(glmobj,newdata=dd,type='response'); #Remember to check the length of yy before proceeding!

# Draw the contour corresponding to the boundary of the classifier (Pr=0.5)
contour(xx1,xx2,matrix(yy,200,200),levels=.5,add=TRUE,drawlabels=FALSE,lwd=4,col='grey50');

# Fit linear discriminant
ldaobj <- lda(y~x1+x2);
# Again we have to predict over a grid
zz <- predict(ldaobj, newdata=dd)$posterior[,1]; #Check the help manual for the list of returned values
# Plot contours
contour(xx1,xx2,matrix(zz,200,200),levels=.5,add=TRUE,drawlabels=FALSE,lwd=4,col='maroon');


# Let's change a few points...
par(mfrow=c(1,2));
plot(x1,x2,pch=19,col=y+1,cex=1.3,xlab="Sepal",ylab="Petal",xlim=c(3,8),ylim=c(3,8));
contour(xx1,xx2,matrix(yy,200,200),levels=.5,add=TRUE,drawlabels=FALSE,lwd=4,col='grey50');
contour(xx1,xx2,matrix(zz,200,200),levels=.5,add=TRUE,drawlabels=FALSE,lwd=4,col='maroon');

x1[68:69] <- c(3,3);
x2[68:69] <- c(7.5,7.7); #Then run the above again, from plot(...) to the line before par(...)

