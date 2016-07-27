# Function that plots the piecewise equations along with the dataset

plot.pwl <- function(pwl, data){
  if(class(pwl)!="pwl") stop("Need to provide the piecewise linear equations")
  
  x <- data[,1]
  y <- data[,2]
  
  maxX <- max(abs(x))
  minX <- -maxX
  
  #plot the data
  plot(x,y, ylim=c(-0.1, 1.1), xlim=c(minX, maxX))
  
  BP <- pwl$BreakPoints
  noOfBP <- length(pwl$BreakPoints)
  equations <- list()
  
  i <- 1
  repeat{
    if(i == 1){
      # plotting the first segment
      curve(pwl$coeffs[1,i] + pwl$coeffs[2,i]*x, add=T, from=minX, to=BP[i], col="red")
      equation <- paste0("y = ", round(pwl$coeffs[1,i], 4), "+", round(pwl$coeffs[2,i], 4), "x, for x < ", round(BP[i],4))
      abline(v=BP[i], lty=3)
    }else if(i == noOfBP+1){
      # plotting the last segment
      curve(pwl$coeffs[1,i] + pwl$coeffs[2,i]*x, add=T, from=BP[noOfBP], to=maxX, col="red")
      equation <- paste0("y = ", round(pwl$coeffs[1,i], 4), "+", round(pwl$coeffs[2,i], 4), "x, for x > ", round(BP[noOfBP],4))
    }else{
      # plotting all the other segments
      curve(pwl$coeffs[1,i] + pwl$coeffs[2,i]*x, add=T, from=BP[i-1], to=BP[i], col="red")
      equation <- paste0("y = ", round(pwl$coeffs[1,i], 4), "+", round(pwl$coeffs[2,i], 4), "x, for ", round(BP[i-1],4), " < x < ", round(BP[i], 4))
      abline(v=BP[i], lty=3)
    }
    equations <- c(equations, equation)
    i <- i+1
    if(i>noOfBP+1) break()
  }
  error <- paste0("MSE = ", pwl$mse)
  legend("topleft", legend = c(equations, error), text.col=c(rep(c("black"), noOfBP+1), "red"))
  text(x = BP, y = 0, labels = c(round(BP, 4)), pos=1)

}