# Function that gets the "left hand side" equation for each node.
# In order the maintain continuity of the curve, the intercepts of the 
# lines become the breakpoints.
# The function takes a pwl and the data set and returns the pwl with the left
# hand side equation, "new" breakpoints, fitted values and mse.
# The left hand side equations have the same gradient as the right hand side equations
# but with negative intercept values.

getfullpwl <- function(pwl, data, constraint){
  
  if(class(pwl)!="pwl") stop("Need to provide the piecewise linear equations")
  
  newpwl <- list(coeffs = matrix, BreakPoints = c(), fitted = matrix, residuals = matrix, mse = 0)
  #equations <- matrix()
  noOfBP <- length(pwl$BreakPoints)
  
  #copy and duplicate the equations from the pwl class
  newcoeffs <- cbind(pwl$coeffs[,-1], pwl$coeffs)
  
  #replace the intercept of the left hand side equation to negative values
  newcoeffs[1,] <- newcoeffs[1,] + constraint
  newcoeffs["(Intercept)", 1:noOfBP] <- ((constraint*2)-newcoeffs["(Intercept)", 1:noOfBP])
  newcoeffs <- newcoeffs[,sort.list(newcoeffs["(Intercept)",])]
  
  #find "new" breakpoints by finding intercepts between all lines
  newBP <- c()
  i <- 1
  repeat{
    BP <- findIntercept(newcoeffs["(Intercept)",i], newcoeffs["x1",i], newcoeffs["(Intercept)", i+1], newcoeffs["x1", i+1])
    newBP <- c(newBP, BP)
    
    i <- i+1
    if(i>(2*noOfBP)) break()
  }
  
  newBP <- sort(newBP)
  
  newpwl$BreakPoints <- newBP
  newpwl$coeffs <- newcoeffs
  
  class(newpwl) <- "pwl"
  
  newpwl$fitted <- predicts.pwl(newpwl, data)
  newpwl$residuals <- data[,2] - newpwl$fitted
  newpwl$mse <- mean(newpwl$residuals^2)
  
  newpwl
}