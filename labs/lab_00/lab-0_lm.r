linear <- function(y,X){
	beta <- solve(t(X)%*%X,t(X)%*%y);
	ehat <- y-X%*%beta;
	sigma <- sqrt(t(ehat)%*%ehat/(length(y)-ncol(X)));
	return(list(beta,sigma));
}

#Fictitious data
X <- cbind(c(1,1,1,1,1),c(1.6,2.2,2.8,-0.5,-1.3),c(4,8,6,3,5));
y <- c(4,5,7,3,2);
linear(y,X);

#Compare with lm
summary(lm(y~X-1)); #Why X-1?

#trade.union data
library(SemiPar);
data(trade.union);
x <- trade.union;
lm(wage~age+years.educ,data=x);

Xmat <- as.matrix(cbind(1,x[,c(7,1)])); #Design matrix (with intercept). Why "as.matrix"?
yvec <- x[,6]; #Response vector
linear(yvec,Xmat);

#Fitted parameters: Intercept: -5.534, age: 0.105, years.educ: 0.821