#2.1 Installing and loading packages, and running predefined functions
#For instance I want to plot histogram using the function truehist
x <- runif(1000,0,1); #Generate 1000 standard uniform r.v.
x;
truehist(x);

#Oops I need to load the package MASS!
library(MASS);
truehist(x);
?truehist #This shows the help file for the function truehist


#2.2 Manipulating matrices
y1 <- matrix(c(1,2,3,4,5,6,7,8,9,0),nrow=2,ncol=5);y1; #by default it fills by column
y2 <- matrix(c(1,2,3,4,5,6,7,8,9,0),byrow=T,nrow=2,ncol=5);y2;
y3 <- cbind(y1,y2);y3;
y4 <- rbind(y1,y2);y4;

y5 <- seq(0,10,by=2.5);y5;
y6 <- seq(0,10,length=5);y6;

y7 <- c(1,2)%*%t(c(2,1));y7;
y8 <- solve(y7+diag(2));y8;

y9 <- colSums(y8);y9; #Note that y9 is NOT a row vector
y10 <- rowSums(y8);y10;

y4;
y4[3,4]; #extract one element
y4[3:4,4:5];
y4[3:4,c(1,3,5)];
y4[1,]; #extract one row. Note that it turns into a (column) vector
y4[-1,];


#2.3 Writing and running your own functions
#A very simple function to solve quadratic equations
myfunc <- function(A,B,C){ #Solve equations of the form Ax^2+Bx+C=0
	discrim <- B^2-4*A*C;
	if(discrim < 0) stop("No real roots!");
	r1 <- (-B-sqrt(discrim))/2/A;
	r2 <- (-B+sqrt(discrim))/2/A;
	return(c(r1,r2));
}

myfunc(3,6,2);
myfunc(3,4,5);
r1; #Why?

f1 <- function(z1){return(z1+z2);}
z2 <- 5;
f1(1);

f2 <- function(z1){z2 <- 3; return(z1+z2);}
z2 <- 5;
f2(1); #What happens?


#2.5 Debugging
#The following creates a matrix of normal r.v. with mean equal to the row number and unit variance
mat <- matrix(0,nrow=10000,ncol=5);
for(i in 1:10000){
	mat[i,] <- rnorm(5,i,1); #Note how I push values into the i-th row of mat
	if(i%%1000==0)print(mat[i,]); #Print every 1000th row
}

debug(myfunc);
myfunc(6,4,-3);
undebug(myfunc);

