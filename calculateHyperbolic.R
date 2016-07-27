# CalculateHyperbolic.R
# Function that takes the weighted input and return the matrix with weighted input and hyperbolic tangant function

calculateHyperbolic <- function(data){
  
  hyperbolicdata <-  (exp(data)-exp(-data))/(exp(data)+exp(-data))
  
  hyperbolicdata
}