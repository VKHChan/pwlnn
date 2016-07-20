# Function that predicts the pwlnn output.
# The function takes: Input data, output data, weighte of the hidden and output neurons,
# the set of pwl equations and the activation function.
# The function returns the output given by the ANN model with pwl equations.

predict.pwlnn <- function(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, allpwlfull, actfun = "sigmoid"){
  
  noofNode <- ncol(hiddenW)
  
  # check if all the pwl are given
  if(length(allpwlfull)!= noofNode) stop("The number of pwl equation given does not match the number of nodes.")
  
  # check if all the input paramenters are provide
  if(missing(Inputdata)|| missing(ANNOutput) || missing(hiddenW) || missing(outputW)) stop("Must provide Input data, output data, hidden weights and output weights.\n")
  
  # check if the Input and output have the same number of entries
  if(nrow(Inputdata)!= nrow(ANNOutput)) stop("Length of Input and output data do not match.\n")
  
  # check if there are enough number of weights for the hidden neurons
  # number of weights per hidden neuron should be equals to number of input nodes + bias
  if(nrow(hiddenW) != (ncol(Inputdata)+1)) stop("Weights per hidden neuron does not match the number of input.\n")
  
  # check if there are enough number of weights for the output neurons
  # number of weights per output neuron should be equals to number of hidden nodes + bias
  if(nrow(outputW) != (ncol(hiddenW)+1)) stop("Weights per hidden neuron does not match the number of input.\n")
  
  # check what activation was given
  if(actfun=="sigmoid"){
    constraint <- 0.5
    activation <- "calculateSigmoid"
  }else if(actfun=="hypoberlic"){
    constraint <- 0
    activation <- "calculateHyperbolic"
  }else{
    stop("Activation function can be either be sigmoid or hypobolic.\n Acticaation function by default is sigmoid function")
  }
  
  # get the activation function
  activationfunction <- get(activation)
  # calculate weighted input
  weightedInput <- as.data.frame(calculateWeightedValues(Inputdata, hiddenW))
  sigmoid <- activationfunction(weightedInput)
  
  nnoutput <- rowSums(calculateWeightedValues(sigmoid, outputW))
  #nnoutput <- denormalize(ANNOutput, nnoutput, -1, 1)
  #nnoutput <- denormalize(ActualOutput, nnoutput, -1, 1)
  
  
  # calculate the pwl value of each node
  pwlvalue <- matrix(nrow = nrow(weightedInput), ncol=ncol(weightedInput))
  i <- 1
  repeat{
    nodename <- paste0("node", i)
    node <- weightedInput[,i]
    pwl <- allpwlfull[[nodename]]
    
    pwlvalue[,i] <- predicts.pwl(pwl, node)
    
    i <- i+1
    if(i>noofNode) break()
  }
  
  pwlout <- rowSums(calculateWeightedValues(pwlvalue, outputW))
  #pwlout <- denormalize(ANNOutput, pwlout, -1, 1)
  #pwlout <- denormalize(ActualOutput, pwlout, -1, 1)
  
  result <- as.data.frame(cbind(nnoutput, pwlout))
  
  result
}