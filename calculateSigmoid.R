# CalculateSigmoid.R
# Function that takes the weighted input and return the matrix with weighted input and sigmoid function
# ** can be more generalized and consider other functions too
calculateSigmoid <- function(data){
  
  sigmoiddata <-  1/(1+exp(-data))
  
  sigmoiddata
}