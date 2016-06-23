# Function to determine the x-intercept of two given lines
findIntercept <- function(intercept1, gradient1, intercept2, gradient2){
  
  interceptx <- (intercept2-intercept1)/(gradient1-gradient2)
  intercepty <- intercept1+ gradient1*interceptx
  interceptx
  
}