# Function that is used to print and save all the plots for the hidden nodes with the pwl functions.
# Plots will be saved as PNG format.

plot.pwlnn.nodes <- function(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, allpwlfull, actfun){
  noOfnode <- ncol(hiddenW)
  
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
  
  # print the pwl graphs for each node
  i <- 1
  repeat{
    node <- paste0("node", i)
    data <- cbind(weightedInput[,i], sigmoid[,i])
    
    title <- paste0("Node ", i)
    png(paste0(title,".png"), width = 900, height = 600)
    plot.pwl(allpwlfull[[node]], data)
    title(main = title, xlab = "Weighted Input", ylab = "Activated Values")
    
    dev.off()
    
    i <- i+1
    if(i>noOfnode) break()
    
  }
}