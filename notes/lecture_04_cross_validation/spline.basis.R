spline.basis <- function(x, nknots = 1, knots = NULL,  degree = 1)
  # Generate the X matrix for linear, quadratic, … splines (continuous derivatives).
  # If knots is given a vector of values, these values are used as the knots
  # and nknots is ignored.
  # If knots is null, then nknots is the number of knots, and the knots are 
  # generated as the quantiles of x.
  # degree is 1 for linear splines, 2 for quadratic splines, etc.  
  # The implied model is continuous and has continuous derivatives up to degree - 1.   
  # Returns the X matrix: polynomial terms in x followed by basis functions in x.
{
  if (is.null(knots)) 
    knots <- as.numeric( quantile(x, (1:nknots) / (nknots + 1)) )
  else
    nknots = length(knots)
  
  # Create the X matrix of covariates / explanatory variables
  x.mat <- matrix(0, nrow = length(x), ncol = degree + nknots)
  # Polynomial terms in the first columns
  for (j in 1:degree)
    x.mat[, j] <- x^j
  # Further columns for spline basis functions
  for(k in 1:nknots) 
    x.mat[, degree+k] <- (pmax(x - knots[k], 0))^degree
  
  return(x.mat)
}