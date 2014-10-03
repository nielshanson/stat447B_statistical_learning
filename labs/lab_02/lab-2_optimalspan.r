#Effect of span on fitting/predictive ability
#Note: only the part on "prerr.test" is required for submission
spanvec <- seq(0.1,0.7,by=0.02); #Set up the vector of spans
prerr.train <- rep(0,length(spanvec)); #Initial storage vectors
prerr.test <- rep(0,length(spanvec));

for(i in 1:length(spanvec)){
	fit1 <- loess(ytrain~xtrain,span=spanvec[i],degree=0);
	pr.ytrain <- predict(fit1); #"Predict" the training set
	prerr.train[i] <- sum((ytrain-pr.ytrain)^2);
	
	pr.ytest <- predict(fit1,xtest); #Predict the test set
	prerr.test[i] <- sum((ytest-pr.ytest)^2);
}

plot(spanvec,prerr.train,type="l",lwd=2,col="red",ylim=c(0,120000),xlab="Span",ylab="Sum of squared errors");
lines(spanvec,prerr.test,type="l",lwd=2,col="blue");

#Which span is optimal?
lowindex <- which(prerr.test==min(prerr.test));
spanvec[lowindex]; #0.16
abline(v=spanvec[lowindex],lty=2,lwd=2);
points(spanvec[lowindex],prerr.train[lowindex],pch=21,lwd=6,col="red");
points(spanvec[lowindex],prerr.test[lowindex],pch=21,lwd=6,col="blue");

#Conclusion: Error sum of squares on the TRAINING set is NOT a good measure of the predictive ability of a model!