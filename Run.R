library(xlsx)
library(ggplot2)
library(strucchange)
library(psych)
library(stats)
library(data.table)
library(party)
library(rpart)
library(rpart.plot)

############################################################
InputData <- read.table("InputData.txt", sep = "\t", header = T)
InputData <- na.omit(InputData)
WeightMatrix <- read.table("WeightMatrix.txt", sep = "\t", header = T)
WeightMatrix <- na.omit(WeightMatrix)
OutputData <- read.table("OutputData.txt", sep="\t", header = T)
OutputData <- na.omit(OutputData)
OutputWeight <- read.table("OutputWeightMatrix.txt", sep="\t", header = T)
OutputWeight <- na.omit(OutputWeight)

weightedInput <- as.data.frame(calculateWeightedValues(InputData, WeightMatrix))

Node1 <- cbind(abs(weightedInput$Node.1), calculateSigmoid(abs(weightedInput$Node.1)))
Node2 <- cbind(abs(weightedInput$Node.2), calculateSigmoid(abs(weightedInput$Node.2)))
Node3 <- cbind(abs(weightedInput$Node.3), calculateSigmoid(abs(weightedInput$Node.3)))
Node4 <- cbind(abs(weightedInput$Node.4), calculateSigmoid(abs(weightedInput$Node.4)))
Node5 <- cbind(abs(weightedInput$Node.5), calculateSigmoid(abs(weightedInput$Node.5)))
Node6 <- cbind(abs(weightedInput$Node.6), calculateSigmoid(abs(weightedInput$Node.6)))
Node7 <- cbind(abs(weightedInput$Node.7), calculateSigmoid(abs(weightedInput$Node.7)))
Node8 <- cbind(abs(weightedInput$Node.8), calculateSigmoid(abs(weightedInput$Node.8)))
Node9 <- cbind(abs(weightedInput$Node.9), calculateSigmoid(abs(weightedInput$Node.9)))

#############################################################
Testpwl1 <- pwl(Data, noOfBP = 1, l = 1)

# Test Data###################################
x <- seq(0, 30, 0.1)
y <- calculateSigmoid(x)
Data <- cbind(x, y)

i <- 1
repeat{
  ptm <- proc.time()
  Testpwl1 <- pwl(Data, noOfBP = i, l = 1)
  print(paste0("no fo BP = ", i))
  print(proc.time() - ptm)
  
  i <- i+1
  if(i>2) break()
}

ptm <- proc.time()
Testpwl1 <- pwl(Data, maxBP = 2, l = 1)
proc.time() - ptm

ptm <- proc.time()
Testpwl1 <- pwl(Data, maxBP = 5, l = 1)
proc.time() - ptm

#Node 1##################################
#x <- c(0, seq(min(Node1[,1]), max(Node1[,1]), 0.05))
#x <- seq(min(Node1[,1]), max(Node1[,1]), 0.1)
#Taking every 3rd data points as sample
#Data <- Node1[sort.list(Node1[,1]),]
#x <- Data[seq(1, nrow(Node1), 3),1]
#y <- calculateSigmoid(x)
#Data <- cbind(x,y)
Data <- Node1

range(Data[,1])
mean(Data[,1])
var(Data[,1])
median(Data[,1])

ptm <- proc.time()
Testpwl1 <- pwl(Data, noOfBP = 2, l = 1)
proc.time() - ptm
plot.pwl(Testpwl1$pwl2,Data)

write.table(Testpwl1$pwl2$coeffs, file = "pwlNode1Coef.txt", sep = "\t")
write.table(Testpwl1$pwl2$BreakPoints, file = "pwlNode1BP.txt", sep = "\t")

Testpwl1$pwl2$BreakPoints
plot.pwl(Testpwl1$pwl2,Node1)
mean((predicts.pwl(Testpwl1$pwl2, Node1)- Node1[,2])^2)
1-(sum((Node1[,2]-predicts.pwl(Testpwl1$pwl2, Node1))^2)/sum((Node1[,2]-mean(Node1[,2]))^2))
plot(Node1[,2], predicts.pwl(Testpwl1$pwl2, Node1))

#Node 2###################################
#x <- seq(min(Node2[,1]), max(Node2[,1]), 0.05)
#Data <- Node2[sort.list(Node2[,1]),]
#x <- Data[seq(1, nrow(Node2), 3),1]
#y <- calculateSigmoid(x)
#Data <- cbind(x,y)
Data <- Node2

range(Data[,1])
mean(Data[,1])
var(Data[,1])
median(Data[,1])

ptm <- proc.time()
Testpwl1 <- pwl(Data, noOfBP = 2, l = 1)
proc.time() - ptm
plot.pwl(Testpwl1$pwl2, Data)

write.table(Testpwl1$pwl2$coeffs, file = "pwlNode2Coef.txt", sep = "\t")
write.table(Testpwl1$pwl2$BreakPoints, file = "pwlNode2BP.txt", sep = "\t")

Testpwl1$pwl2$BreakPoints
plot.pwl(Testpwl1$pwl2,Node2)
mean((predicts.pwl(Testpwl1$pwl2, Node2)- Node2[,2])^2)
1-(sum((Node2[,2]-predicts.pwl(Testpwl1$pwl2, Node2))^2)/sum((Node2[,2]-mean(Node2[,2]))^2))
plot(Node2[,2], predicts.pwl(Testpwl1$pwl2, Node2))

#Node 3###################################
#x <- seq(min(Node3[,1]), max(Node3[,1]), 0.05)
#Data <- Node3[sort.list(Node3[,1]),]
#x <- Data[seq(1, nrow(Node3), 3),1]
#y <- calculateSigmoid(x)
#Data <- cbind(x,y)
Data <- Node3

range(Data[,1])
mean(Data[,1])
var(Data[,1])
median(Data[,1])

ptm <- proc.time()
Testpwl1 <- pwl(Data, noOfBP = 2, l = 1)
proc.time() - ptm
plot.pwl(Testpwl1$pwl2, Data)

write.table(Testpwl1$pwl2$coeffs, file = "pwlNode3Coef.txt", sep = "\t")
write.table(Testpwl1$pwl2$BreakPoints, file = "pwlNode3BP.txt", sep = "\t")

Testpwl1$pwl2$BreakPoints
plot.pwl(Testpwl1$pwl2,Node3)
mean((predicts.pwl(Testpwl1$pwl2, Node3)- Node3[,2])^2)
1-(sum((Node3[,2]-predicts.pwl(Testpwl1$pwl2, Node3))^2)/sum((Node3[,2]-mean(Node3[,2]))^2))
plot(Node3[,2], predicts.pwl(Testpwl1$pwl2, Node3))

#Node 4###################################
#x <- seq(min(Node4[,1]), max(Node4[,1]), 0.05)
#Data <- Node4[sort.list(Node4[,1]),]
#x <- Data[seq(1, nrow(Node4), 3),1]
#y <- calculateSigmoid(x)
#Data <- cbind(x,y)
Data <- Node4

range(Data[,1])
mean(Data[,1])
var(Data[,1])
median(Data[,1])

ptm <- proc.time()
Testpwl1 <- pwl(Data, noOfBP = 2, l = 1)
proc.time() - ptm
plot.pwl(Testpwl1$pwl2, Data)

write.table(Testpwl1$pwl2$coeffs, file = "pwlNode4Coef.txt", sep = "\t")
write.table(Testpwl1$pwl2$BreakPoints, file = "pwlNode4BP.txt", sep = "\t")

Testpwl1$pwl2$BreakPoints
plot.pwl(Testpwl1$pwl2,Node4)
mean((predicts.pwl(Testpwl1$pwl2, Node4)- Node4[,2])^2)
1-(sum((Node4[,2]-predicts.pwl(Testpwl1$pwl2, Node4))^2)/sum((Node4[,2]-mean(Node4[,2]))^2))
plot(Node4[,2], predicts.pwl(Testpwl1$pwl2, Node4))

#Node 5###################################
#x <- seq(min(Node5[,1]), max(Node5[,1]), 0.05)
#Data <- Node5[sort.list(Node5[,1]),]
#x <- Data[seq(1, nrow(Node5), 3),1]
#y <- calculateSigmoid(x)
#Data <- cbind(x,y)
Data <- Node5

range(Data[,1])
mean(Data[,1])
var(Data[,1])
median(Data[,1])

ptm <- proc.time()
Testpwl1 <- pwl(Data, noOfBP = 2, l = 1)
proc.time() - ptm
plot.pwl(Testpwl1$pwl2, Data)

write.table(Testpwl1$pwl2$coeffs, file = "pwlNode5Coef.txt", sep = "\t")
write.table(Testpwl1$pwl2$BreakPoints, file = "pwlNode5BP.txt", sep = "\t")

Testpwl1$pwl2$BreakPoints
plot.pwl(Testpwl1$pwl2,Node5)
mean((predicts.pwl(Testpwl1$pwl2, Node5)- Node5[,2])^2)
1-(sum((Node5[,2]-predicts.pwl(Testpwl1$pwl2, Node5))^2)/sum((Node5[,2]-mean(Node5[,2]))^2))
plot(Node5[,2], predicts.pwl(Testpwl1$pwl2, Node5))

#Node 6###################################
#x <- seq(min(Node6[,1]), max(Node6[,1]), 0.05)
#Data <- Node6[sort.list(Node6[,1]),]
#x <- Data[seq(1, nrow(Node6), 3),1]
#y <- calculateSigmoid(x)
#Data <- cbind(x,y)
Data <- Node6

range(Data[,1])
mean(Data[,1])
var(Data[,1])
median(Data[,1])

ptm <- proc.time()
Testpwl1 <- pwl(Data, noOfBP = 2, l = 1)
proc.time() - ptm
plot.pwl(Testpwl1$pwl2, Data)

write.table(Testpwl1$pwl2$coeffs, file = "pwlNode6Coef.txt", sep = "\t")
write.table(Testpwl1$pwl2$BreakPoints, file = "pwlNode6BP.txt", sep = "\t")

Testpwl1$pwl2$BreakPoints
plot.pwl(Testpwl1$pwl2,Node6)
mean((predicts.pwl(Testpwl1$pwl2, Node6)- Node6[,2])^2)
1-(sum((Node6[,2]-predicts.pwl(Testpwl1$pwl2, Node6))^2)/sum((Node6[,2]-mean(Node6[,2]))^2))
plot(Node6[,2], predicts.pwl(Testpwl1$pwl2, Node6))

#Node 7###################################
ptm <- proc.time()
Testpwl1 <- pwl(Node7, noOfBP = 1, l = 1)
proc.time() - ptm

Testpwl1$pwl1$BreakPoints
plot.pwl(Testpwl1,Node7)
mean((predicts.pwl(Testpwl1, Node7)- Node7[,2])^2)
1-(sum((Node7[,2]-predicts.pwl(Testpwl1, Node7))^2)/sum((Node7[,2]-mean(Node7[,2]))^2))
plot(Node7[,2], predicts.pwl(Testpwl1, Node7))

#Node 8###################################
ptm <- proc.time()
Testpwl1 <- pwl(Node8, noOfBP = 1, l = 1)
proc.time() - ptm

Testpwl1$pwl1$BreakPoints
plot.pwl(Testpwl1,Node8)
mean((predicts.pwl(Testpwl1, Node8)- Node8[,2])^2)
1-(sum((Node8[,2]-predicts.pwl(Testpwl1, Node8))^2)/sum((Node8[,2]-mean(Node8[,2]))^2))
plot(Node8[,2], predicts.pwl(Testpwl1, Node8))

#Node 9###################################
ptm <- proc.time()
Testpwl1 <- pwl(Node9, noOfBP = 1, l = 1)
proc.time() - ptm

Testpwl1$pwl1$BreakPoints
plot.pwl(Testpwl1,Node9)
mean((predicts.pwl(Testpwl1, Node9)- Node9[,2])^2)
1-(sum((Node9[,2]-predicts.pwl(Testpwl1, Node9))^2)/sum((Node9[,2]-mean(Node9[,2]))^2))
plot(Node9[,2], predicts.pwl(Testpwl1, Node9))

################################################################
range(Node1[,1])
range(Node2[,1])
range(Node3[,1])
range(Node4[,1])
range(Node5[,1])
range(Node6[,1])
range(Node7[,1])
range(Node8[,1])
range(Node9[,1])

mean(Node1[,1])
var(Node1[,1])
mean(Node2[,1])
var(Node2[,1])
mean(Node3[,1])
var(Node3[,1])
mean(Node4[,1])
var(Node4[,1])
mean(Node5[,1])
var(Node5[,1])
mean(Node6[,1])
var(Node6[,1])
mean(Node7[,1])
var(Node7[,1])
mean(Node8[,1])
var(Node8[,1])
mean(Node9[,1])
var(Node9[,1])

median(Node1[,1])
median(Node2[,1])
median(Node3[,1])
median(Node4[,1])
median(Node5[,1])
median(Node6[,1])
median(Node7[,1])
median(Node8[,1])
median(Node9[,1])
###########################
ptm <- proc.time()
ThreePiece<- ANNPWLApproximation(InputData, OutputData, WeightMatrix, OutputWeight, 3)
proc.time() - ptm

#ptm <- proc.time()
##will be used later to find 5P
#SSRMatrix <- calculateSSRMatrix(weightedInput.node1, 5)
#proc.time() - ptm
Result <- ApplyPWLApproximation(InputData, OutputData, WeightMatrix, OutputWeight, ThreePiece)
Error(Result)

YEquations <- calculateYEqnAll(InputData, WeightMatrix, ThreePiece)
YEquationsUnique <- YUnique(YEquations)
YClass <- FindYClass(InputData, WeightMatrix, ThreePiece)
##############################################################


############################################################
# Decision Tree on Y "classes"
############################################################
DataYclass <- read.xlsx("Input with Y Class.xlsx", sheetName = "Sheet1")
DataYclass <- DataYclass[, -1]

#create formula for decision tree
lengthFormular <- length(attributes(DataYclass)$names)
myFormula <- formula(paste(names(DataYclass)[lengthFormular], "~", paste(names(DataYclass)[-lengthFormular], collapse=" + ")))

#use rpart() to generate decistion tree.
#it is a classification problem now
YclassTree <- rpart(myFormula, DataYclass, method = "class")

#write results ("rules") given by decision tree to file
sink("TreeOutput.txt")
print(YclassTree)
sink()

png("Decision Tree with Regression equations as class.png", width= 900, height = 600)
prp(YclassTree)
dev.off()

plot(YclassTree)
text(YclassTree, use.n=TRUE)
print(YclassTree)
printcp(YclassTree)
plotcp(YclassTree)

#############################################################
