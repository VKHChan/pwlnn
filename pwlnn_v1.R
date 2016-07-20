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
# (defult maxBP = 6), the desired error (mse percentage difference with the ANN model, default = 20%), if the user wants to use
# sampling technqiue for a faster algorithm (default sampling=FALSE), and the sampling size user wants to use (default = 300)
# If sampling = TRUE, the algorithm will use a sampling data (size = 600) for approximation.

# The function returns:


pwlnn <- function(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, actfun="sigmoid", l=NULL, maxBP=NULL, error=NULL, sampling=FALSE, samplesize=NULL, ...){
  
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
  
  # check if an even number of BP is given
  if(!is.null(maxBP) && (maxBP%%2 != 0)) stop("Max number BP has to be an even number.")
  
  # Set l to default value if user did not specify
  if(is.null(l)){ 
    l<- 1
    print("Default value of l is used, l = 1.")
  }
  print(paste0("Value of l is ", l))
  
  # set maxBP to default value if user did not specify
  if(is.null(maxBP)) {
    maxBP <- 6
    print("Default value of maxBP is used, maxBP = 6.")
  }
  print(paste0("Value of maxBP is ", maxBP ))
  
  # set error to default value if user did not specify
  if(is.null(error)) {
    error <- 20
    print("Default value of error is used, maxBP = 20%.")
  }
  print(paste0("Value of error is ", error ))
  
  # set constraints for the given activation function
  if(actfun=="sigmoid"){
    constraint <- 0.5
    activation <- "calculateSigmoid"
  }else if(actfun=="hyperbolic"){
    constraint <- 0
    activation <- "calculateHyperbolic"
  }else{
    stop("Activation function can be either be sigmoid or hyperbolic.\n Acticaation function by default is sigmoid function")
  }
  
  # get the activation function
  activationfunction <- get(activation)
  
  # get the number of node in the given ANN model
  noofNode <- ncol(hiddenW)
  
  weightedInput <- as.data.frame(calculateWeightedValues(InputData, hiddenW))
  
  # use absolute weighed input values for approximation only
  sampledata <- abs(weightedInput)
  
  # sort the weighted input
  sampledata <- as.data.frame(lapply(sampledata, sort, decreasing=F))
  
  if(sampling==FALSE){
    # if the user does not want sampling, do nothing
    # print a warning message
    print("Sampling data is advised for data size greater than 800.")
  }else{
    if(is.null(samplesize)){
      # sample size is set to default if it is not specified by user
      samplesize <- 300
      print("Default value of sample size is used, sample size = 300.")
    }
    
    # Check if the data size is greater than sample size
    if(nrow(Inputdata) < samplesize){
      print("Data size is smaller than sample size. Cannot generate sample data.")
    }else{
      # take samples of the data
      sampledata <- takesample(sampledata, samplesize)
    }
  }
  
  # include 0 at as the first data point
  sampledata <- rbind(0, sampledata)
  noBP <- maxBP/2
  
  # find piecewise linear equations of each node with BP up to maxBP
  allpwlhalf <- list()
  i <- 1
  repeat{
    name <- paste0("node",i)
    
    node <- cbind(sampledata[,i], activationfunction(sampledata[,i])-constraint)
    allpwlhalf[[name]] <- pwl.nn(data = node, l = l, maxBP = noBP)
    
    i <- i+1
    if(i>noofNode) break()
  }
  
  # Get the full equations:
  # since we are using the absolute values of weighted input the perform the approximation
  # the equations in allpwl contains only half (the right hand side) of the pwl equations
  
  allpwlfull <- list()
  i <- 1
  repeat{
    name <- paste0("node", i)
    data <- cbind(weightedInput[,i], activationfunction(weightedInput[,i]))
    
    #copy node i 's half pwl equations
    pwl <- allpwlhalf[[name]]
    
    fullpwl <- list()
    j <- 1
    repeat{
      pwlname <- paste0("pwl",j)
      
      pwlfull <- getfullpwl(pwl[[pwlname]], data, constraint)
      
      # give the pwl a new name, since the number of breakpoint is doubled now
      newpwlname <- paste0("pwl", j*2)
      fullpwl[[newpwlname]] <- pwlfull
      
      j <- j+1
      if(j>length(pwl)) break()
    }
    allpwlfull[[name]] <- fullpwl
    
    i <- i+1
    if(i>noofNode) break()
  }
  
  bestpwl <- findbestpwl(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, actfun, allpwlfull, error)
  
  bestpwl
}