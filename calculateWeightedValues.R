# WeightedInput.R
# function that takes an input matrix and a weight matrix
# and return a weighted input matrix

calculateWeightedValues <- function(Data, WeightMatrix){
  
  #the last row of the weight matrix is the biases
  #add a row of 1's to multiply with the bias
  Data <- cbind(Data, rep(1, nrow(Data)))

  
  Data <- as.matrix(Data)
  WeightMatrix <- as.matrix(WeightMatrix)
  WeightedInput <- Data%*%WeightMatrix
  
  WeightedInput
}
