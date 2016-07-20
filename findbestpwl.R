# Function that is used to find the best set of pwl in that will return a mse% difference of less than 20%
# First, we try if all node with 2BP is sufficient.
# If not, we perform a SA by increaing the breakpoint one node at at a time and see if the best solution could be given
# If SA also fails, we try to find the best combination of nodes
# 
# The function takes the input and output data, the trained ANN model (hidden weights and output weights),
#  and the pwl equations (with different number of breakpoints)

findbestpwl <- function(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, actfun="sigmoid", allpwlfull, error){
  
  #error <- 20
  noOfnode <- ncol(hiddenW)
  maxBP <- length(allpwlfull[[1]])
  noOfBP <- seq(4, maxBP*2, 2)
  
  nnmse <- mean((ANNOutput-ActualOutput)^2)
  
  #first, we try if 2 BP for all nodes is sufficient
  basepwl <- list()
  
  i <- 1
  repeat{
    name <- paste0("node",i)
    basepwl[[name]] <- allpwlfull[[name]]$pwl2
    
    i <- i+1
    if(i>noOfnode) break()
  }
  basepwloutput <- predict.pwlnn(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, basepwl, actfun="sigmoid")
  basemse <- mean((basepwloutput[,2]-ActualOutput)^2)

  mseper <- ((basemse-nnmse)*100)/nnmse
  print(paste0("Base mse % is ", mseper, "%."))
  
  if(mseper < error){
    print("Best solution is given when all node has 2 BP.")
    print(paste0("Mse % given by this solution is ", mseper, "%."))
    
    bestpwl <- basepwl
  }else{
    #otherwise, we perform SA and see if a solution could be found
    SAMatrix <- matrix(ncol=noOfnode, nrow=maxBP-1)
    colnames(SAMatrix) <- names(allpwlfull)
    rownames(SAMatrix) <- names(allpwlfull$node1[-1])
    
    i <- 1
    repeat{
      name <- paste0("node", i)
      nodepwl <- allpwlfull[[name]]
      pwlnames <- names(nodepwl[-1])
      
      j <- 1
      repeat{
        # copy the base pwl equations
        newpwl <- basepwl
        # replace the pwl of node i with the other pwl
        
        pwlname <- pwlnames[j]
        newpwl[[name]] <- nodepwl[[pwlname]]

        newpwloutput <- predict.pwlnn(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, newpwl, actfun="sigmoid")
        newmse <- mean((newpwloutput[,2]-ActualOutput)^2)
        
        #calculate the mse percentage 
        newmseper <- ((newmse-nnmse)*100)/nnmse
        
        SAMatrix[pwlname,i] <- newmseper

        j <- j+1
        if(j>length(pwlnames)) break()
      }
      
      i <- i+1
      if(i>noOfnode) break()
    }
    
    print("SA Matrix")
    print(SAMatrix)
    # if there is a solution given by just increasing BP of one node, return that solution
    minmseInd <- arrayInd(which.min(SAMatrix), dim(SAMatrix))
    minmseper <- SAMatrix[minmseInd]
    
    if(minmseper < error){
      thenode <- colnames(SAMatrix)[minmseInd[,2]]
      thepwl <- rownames(SAMatrix)[minmseInd[,1]]
      
      bestpwl <- basepwl
      bestpwl[[thenode]] <- allpwlfull[[thenode]][[thepwl]]
      
      print(paste0("Best solution is given when ", thepwl, "is used for ", thenode))
      print(paste0("Mse % given by this solution is ", minmseper, "%."))
    }else{
      # first check if any mse given by the SA is less than the base mse
      # if not, then base mse is already the best solution
      if(mseper < minmseper){
        bestpwl <- basepwl
        break()
      }else{
        # otherwise, we need to find the combination of nodes
        # Find the nodes that reduces the mse percentage by more than 5%
        FilterSA <- SAMatrix[, SAMatrix[1,] < mseper*0.95]
        
        # If no nodes decreases the mse percentage by more than 5%
        if(ncol(FilterSA)==0){
          # then, we find all the nodes that are just less than mse percentage
          FilterSA <- SAMatrix[, SAMatrix[1,] < mseper]
        }

        # sort the filtered matrix
        FilterSA <- FilterSA[, sort.list(FilterSA[1,])]
        print("Filter SA Matrix")
        print(FilterSA)
        
        nodesSA <- colnames(FilterSA)

        #if there are only one node
        if(ncol(FilterSA)==1){
          minmseInd <- arrayInd(which.min(FilterSA), dim(FilterSA))
          minmseper <- FilterSA[minmseInd]
          
          thenode <- colnames(FilterSA)[minmseInd[,2]]
          thepwl <- rownames(FilterSA)[minmseInd[,1]]
          
          bestpwl <- basepwl
          bestpwl[[thenode]] <- allpwlfull[[thenode]][[thepwl]]
          
          print(paste0("Best solution is given when ", thepwl, "is used for ", thenode))
          print(paste0("Mse % given by this solution is ", minmseper, "%."))
        }else{
          
          # create a matrix to keep track of the number of BP used per node
          # and the mse percentage given by that combination
          # initialize all nodes to have 2 BP
          bppernode <- matrix(ncol=noOfnode+1, nrow=1, c(1))
          colnames(bppernode) <- c(names(allpwlfull), "mseper")
          
          pwllist <- names(allpwlfull[[1]])
          
          # initialize the first two nodes in the SA node list to use 4 BP
          bppernode[, nodesSA[1]] <- 2
          bppernode[, nodesSA[2]] <- 2
          
          # get the set of pwl equations
          combopwl <- list()
          i <- 1
          repeat{
            node <- colnames(bppernode)[i]
            pwl <- pwllist[[bppernode[1, i]]]
            
            combopwl[[node]] <- allpwlfull[[node]][[pwl]]
            
            i <- i+1
            if(i>noOfnode) break()
          }
          combopwloutput <- predict.pwlnn(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, combopwl, actfun="sigmoid")
          combomse <- mean((combopwloutput[,2]-ActualOutput)^2)
          
          #calculate the mse percentage 
          combomseper <- ((combomse-nnmse)*100)/nnmse
          bppernode[1, "mseper"] <- combomseper
          
          if(combomseper < error){
            bestpwl <- combopwl
            
            print(paste0("Best solution is given when ", nodesSA[1], "and ", nodeSA[2], "uses 4 BP."))
            print(paste0("Mse % given by this solution is ", combomseper, "%."))
            print(bppernode)
          }else{
            # otherwise, we increase the number of breakpoints one by one
            # for all the nodes on the node SA list.
            nextcombopwl <- combopwl
            i <- 1
            repeat{
              # increase the number of BP for the ith node on the SA list
              node <- nodesSA[[i]]
              #print(node)
              # get the previous number of BP for that node
              j <- bppernode[i, node] +1
              repeat{
                pwl <- pwllist[[j]]
                nextcombopwl[[node]] <- allpwlfull[[node]][[pwl]]
                
                nextbppernode <- matrix(ncol=noOfnode+1, nrow=1)
                colnames(nextbppernode) <- c(names(allpwlfull), "mseper")
                
                nextbppernode[1,] <- bppernode[i,]
                nextbppernode[1, node] <- j
                
                nextcombopwloutput <- predict.pwlnn(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, nextcombopwl, actfun="sigmoid")
                nextcombomse <- mean((nextcombopwloutput[,2]-ActualOutput)^2)
                
                #calculate the mse percentage 
                nextcombomseper <- ((nextcombomse-nnmse)*100)/nnmse
                nextbppernode[1, "mseper"] <- nextcombomseper
                
                bppernode <- rbind(as.matrix(bppernode), as.matrix(nextbppernode))
                
                if(nextcombomseper < error){
                  bestpwl <- nextcombopwl
                  print(paste0("Best solution is given with mse% = ", nextcombomseper))
                  print(bppernode)
                  return(bestpwl)
                }
                j <- j+1
                if(j>length(pwllist)) break()
              }
              i <- i+1
              if(i>length(nodesSA)) break()
            }
            # if none of the combination gives a mse percentage less than desired error
            # we return the lowest mse percentage as the best solution
            combominmseper <- matrix(ncol=noOfnode+1, nrow=1)
            colnames(combominmseper) <- c(names(allpwlfull), "mseper")
            
            combominmseper[1,] <- bppernode[which.min(bppernode[, "mseper"]),]
            lowestcombopwl <- list()
            i <- 1
            repeat{
              node <- colnames(combominmseper)[i]
              pwl <- pwllist[[combominmseper[1, i]]]
              
              lowestcombopwl[[node]] <- allpwlfull[[node]][[pwl]]
              
              i <- i+1
              if(i>noOfnode) break()
            }
            bestpwl<- lowestcombopwl
            print(paste0("No solution is found to give mse percentage less than ", error, "%."))
            print(paste0("Best solution is found with a mse percentage of ", combominmseper[1,"mseper"], "%."))
            print(combominmseper)
          }
        }
      }
    } 
  }
  bestpwl
}