# Function that finds the Y Equations and it's class of the given data sets and pwl equations
# Input to the functions are: Inputdata, hidden weights, outputweights, and the set of pwl equations
# Output of the function is the list of Y Equation for each input data entry and it's class.

getYEquations <- function(Input, hiddenW, outputW, bestpwl){
  
  noOfdata <- nrow(Input)
  noOfnode <- ncol(hiddenW)
  noOfattribute <- ncol(Input)
  
  YEquations <- matrix(nrow = noOfdata, ncol = noOfattribute +1, c(0))
  colnames(YEquations) <- c(colnames(Input), "constant")
  
  # this is used to keep track of the equation given by a node
  nodeYEquation <- matrix()

  #calulate the weighted input
  weightedInput <- calculateWeightedValues(Input, hiddenW)
  
  i <- 1
  repeat{
    totalYEquation <- matrix(nrow=1, ncol=noOfattribute+1, c(0))
    # take input i
    #thisInput <- Input[i,]
    
    j <- 1
    repeat{

      nodeYEquation <- matrix(nrow=1, ncol=nrow(hiddenW), hiddenW[,j])
      
      # take the weighted value of node j
      value <- weightedInput[i, j]
      
      # find the corresponding pwl
      node <- paste0("node", j)
      pwlBP <- bestpwl[[j]]$BreakPoints
      pwlcoeff <- bestpwl[[j]]$coeffs
      
      noOfBP <- length(pwlBP)

      # check if the weighted value locates in the first section
      if(value < pwlBP[1]){
        nodeYEquation<- pwlcoeff[2,1]*nodeYEquation
        nodeYEquation[,noOfattribute+1] <- pwlcoeff[1,1] + nodeYEquation[,noOfattribute+1] 
      }else if(value >= pwlBP[noOfBP]){
        # check if the weighted value locates in the last section
        nodeYEquation <- pwlcoeff[2,noOfBP+1]*nodeYEquation
        nodeYEquation[,noOfattribute+1] <- pwlcoeff[1,noOfBP+1] + nodeYEquation[,noOfattribute+1]
      }else{
        k <- 1
        repeat{
          if(value > pwlBP[k] & value <= pwlBP[k+1]){
            nodeYEquation <- pwlcoeff[2,k+1]*nodeYEquation
            nodeYEquation[,noOfattribute+1] <- pwlcoeff[1,k+1] + nodeYEquation[,noOfattribute+1]
          }
          k <- k +1
          if(k >= noOfBP) break()
        }
      }
      nodeYEquation <- nodeYEquation*outputW[j,]
      totalYEquation <- totalYEquation + nodeYEquation
      
      j <- j+1
      if(j>noOfnode) break()
    }
    
    YEquations[i,] <- as.matrix(totalYEquation[1,,drop=FALSE])
    YEquations[i, "constant"] <- YEquations[i, "constant"] + outputW[noOfnode+1,1]
    
      
    i <- i+1
    if(i>noOfdata) break()
  }

  #find the set of unique Y equations and the "classes"
  YRules<- findUniqueY(YEquations)
  write.xlsx(YRules, "Y Classes.xlsx")
  print(YRules)
  
  noOfYRules <- nrow(YRules)
  
  #assign the input data to their Y classes
  YClass <- matrix(nrow=noOfdata, ncol=1)
  colnames(YClass) <- c("Y Class")
  i <- 1
  repeat{
    j <- 1
    repeat{
      if(all(YEquations[i,]==YRules[j, 1:(noOfattribute+1)])==TRUE){
        YClass[i,] <- YRules[j,"Y Class"]
        break()
      }
      j <- j+1
      if(j>noOfYRules){
        YClass[i,] <- 0
        break()
      }
    }
    i <- i +1
    if(i>noOfdata) break()
  }
  
  YEquations <- cbind(Input, YEquations, YClass)
  write.xlsx(YEquations, "Input with Y Class.xlsx")
  
  YEquations
}