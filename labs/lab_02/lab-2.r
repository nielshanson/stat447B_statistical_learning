#Load the data
motor.train <- read.csv('D:/motorcycle_train.csv',header=T);
motor.test <- read.csv('D:/motorcycle_test.csv',header=T);

xtrain <- motor.train[,1]; ytrain <- motor.train[,2];
xtest <- motor.test[,1]; ytest <- motor.test[,2];


#Exploratory plot
par(mfrow=c(1,2));
plot(xtrain,ytrain); plot(xtest,ytest);


#Fitting local constant, span 0.1 to the training set
fit <- loess(ytrain~xtrain,span=0.1,degree=0);
xt <- seq(min(xtrain),max(xtrain),length=1000); #Grid of values to predict on
yt <- predict(fit,xt);
plot(xtrain,ytrain);
lines(xt,yt,lwd=2);
prerr <- sum((ytest-predict(fit,xtest))^2); #Predictive error SS
prerr;


#Fitting local constant, span 0.7 to the training set
fit2 <- loess(ytrain~xtrain,span=0.7,degree=0);
#xt <- seq(min(xtrain),max(xtrain),length=1000); #Grid of values to predict on
yt2 <- predict(fit2,xt);
lines(xt,yt2,lwd=2,col="red");
prerr <- sum((ytest-predict(fit2,xtest))^2); #Predictive error SS
prerr;


#Local constant vs linear vs quadratic fits
fit1 <- loess(ytrain~xtrain,span=0.7,degree=0);
fit2 <- loess(ytrain~xtrain,span=0.7,degree=1);
fit3 <- loess(ytrain~xtrain,span=0.7,degree=2);
#xt <- seq(min(xtrain),max(xtrain),length=1000); #Grid of values to predict on
yt1 <- predict(fit1,xt);
yt2 <- predict(fit2,xt);
yt3 <- predict(fit3,xt);
plot(xtrain,ytrain);
lines(xt,yt1,lwd=2);
lines(xt,yt2,lwd=2,col="red");
lines(xt,yt3,lwd=2,col="blue"); #local quadratic seems to be better...


#4-fold CV
#Here I use local linear regression (degree=1)
x <- c(xtrain,xtest);
y <- c(ytrain,ytest);
set.seed(100); #Change the seed and you'll probably find another "optimal" span..
ind <- sample(rep(1:4,each=33)); #Run this line and see what it does

spanvec <- seq(0.1,0.7,by=0.02);
prerr <- rep(0,length(spanvec)); #Storage vector
for(i in 1:length(spanvec)){
	prerr.fold <- rep(0,4); #Temporary storage vector
	for(j in 1:4){
		obj <- loess(y~x,subset=(ind!=j),span=spanvec[i],degree=1); #Fitting 3/4 of data
		preval <- predict(obj,x[ind==j]); #Predict on the remaining 1/4
		prerr.fold[j] <- sum((y[ind==j]-preval)^2,na.rm=T);
	}
	prerr[i] <- sum(prerr.fold,na.rm=T);
}

plot(spanvec,prerr,type="o",lwd=2,col="maroon",xlab="Span",ylab="Sum of squared errors");
lowindex <- which(prerr==min(prerr));
spanvec[lowindex]; #0.24
abline(v=spanvec[lowindex],lty=2,lwd=2);
points(spanvec[lowindex],prerr[lowindex],pch=21,lwd=6,col="green");
