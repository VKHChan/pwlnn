# Function that is used to find one breakpoint only, without creating the SSR matrix

findoneBP.nn <- function(data, l){
  result <- list(minssr=0, BP=c())
  
  x <- as.matrix(data[,1])
  y <- as.matrix(data[,2])
  
  breaks <- x[(2+l):(nrow(data)-l-2)]
  ssr <- numeric(length(breaks))
  for(i in 1:length(breaks)){
    x1 <- data[1:i,1]
    x2 <- data[i:nrow(data),1]
    y1 <- data[1:i,2]
    y2 <- data[i:nrow(data),2]
    
    if(ismidsection(x1)==TRUE){
      piecewise1 <- lm(y1~x1+0)
      piecewise2 <- lm(y2~x2)
    }else{
      piecewise1 <- lm(y1~x1)
      piecewise2 <- lm(y2~x2+0)
    }

    #mse[i] <- summary(piecewise1)[6]
    ssr[i] <- sum((piecewise1$residuals)^2)+ sum((piecewise2$residuals)^2)
  }
  ssr <- as.numeric(ssr)
  bp <- breaks[which(ssr==min(ssr))]
  
  result$minssr <- min(ssr)
  result$BP <- c(bp)
  
  result
}