# pwlnn - piecewise linear neural network
# Approximate the nodes of a trained artificial neural network (ANN) models
# and express outputs as multiple linear regression equation
# The function provides a fast approach for large datasets (> 1000 entries)
# by taking samples of the dataset while maintaining the data characteristics.
# For datasets greater with 1000 entries in size, each node will take 
# approximately 10 minutes to approximate.
# For datasets greater with 10000 entries, each node will take approximately
# 2 days to approximate (on the lenovo, 3 days on the dell).

# The function takes: Input data, output data, weights of hidden neurons, weights of output neurals,
# minimum distance (data points) between breakpoints (default l=1), maximum number of BP of each node, 
# (defult maxBP = 3?), the desired error (in terms of R2? mse?, default=?), and if the user wants to use
# sampling technqiue for a faster algorithm (default sampleing=FALSE)
# If sampling = TRUE, the algorithm will use a sampling data (size = 600) for approximation.

# The function returns: 


pwlnn <- function(Inputdata, Outputdata, hiddenW, outputW, actfun="sigmoid", l=NULL, maxBP=NULL, error=NULL, sampling=FALSE, ...){
  
  # check if all the input paramenters are provide
  if(missing(Inputdata)|| missing(Outputdata) || missing(hiddenW) || missing(outputW)) stop("Must provide Input data, output data, hidden weights and output weights.\n")

  # check if the Input and output have the same number of entries
  if(nrow(Inputdata)!= nrow(Outputdata)) stop("Length of Input and output data do not match.\n")
  
  # check if there are enough number of weights for the hidden neurons
  # number of weights per hidden neuron should be equals to number of input nodes + bias
  if(nrow(hiddenW) != (ncol(Inputdata)+1)) stop("Weights per hidden neuron does not match the number of input.\n")
  
  # check if there are enough number of weights for the output neurons
  # number of weights per output neuron should be equals to number of hidden nodes + bias
  if(nrow(outputW) != (ncol(hiddenW)+1)) stop("Weights per hidden neuron does not match the number of input.\n")
  
  # Set l to default value if user did not specify
  if(is.null(l)){ 
    l<- 1
    print("Default value of l is used, l = 1.\n")
  }
  print(paste0("Value of l is ", l))
  
  # set maxBP to default value if user did not specify
  if(is.null(maxBP)) {
    maxBP <- 3
    print("Default value of maxBP is used, maxBP = 3. \n")
  }
  print(paste0("Value of maxBP is ", maxBP ))
  
  # get the number of node in the given ANN model
  noofNode <- ncol(hiddenW)
  
  weightedInput <- as.data.frame(calculateWeightedValues(InputData, WeightMatrix))
  
  # use absolute weighed input values for approximation only
  sampledata <- abs(weightedInput)
  # sort the weighted input
  sampledata <- as.data.frame(lapply(sampledata, sort, decreasing=F))
  
  if(sampling==FALSE){
    # if the user does not want sampling, do nothing
    # print a warning message
    print("Sampling data is advised for data size greater than 800.")
  }else{
    # take samples of the data
    sampledata <- takesample(sampledata)
  }
  
  # find piecewise linear equations of each node with BP up to maxBP
  allpwl <- list()
  i <- 1
  repeat{
    name <- paste0("node",i)
    
    node <- cbind(sampledata[,i], calculateSigmoid(sampledata[,i]))
    allpwl[[name]] <- pwl(data = node, l = l, maxBP = maxBP)
    
    i <- i+1
    if(i>noofNode) break()
  }
  
  
}