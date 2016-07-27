# Function that can be used to print and save the predicted output Vs actual output graphs.
# Three graphs will be given: Actual output Vs PWL output, Actual output Vs ANN output, and Actual output Vs PWL and ANN output.
# Graphs will be saved in png format.

plot.pwlnn.results <- function(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, allpwlfull, actfun){
  # find the pwloutput and calculate the pwlmse, pwlR2, annmse and ammR2
  pwlOutput <- predict.pwlnn(InputData, ANNOutput, ActualOutput, hiddenW, outputW, allpwlfull, actfun)
  pwlMSE <- mean((pwlOutput[,2]-ActualOutput)^2)
  pwlR2 <- 1-(sum((ActualOutput-pwlOutput[,2])^2)/sum((ActualOutput-mean(as.matrix(ActualOutput)))^2))
  annMSE <- mean((ANNOutput-ActualOutput)^2)
  annR2 <- 1-(sum((ActualOutput-ANNOutput)^2)/sum((ActualOutput-mean(as.matrix(ActualOutput)))^2))
  
  # 3 graphs will be plotted: pwl Vs acutal, ann Vs actual, and pwl+ann Vs actual
  
  maxY <- max(ActualOutput) + (0.2*max(ActualOutput))
  minY <- min(ActualOutput) - (0.2*min(ActualOutput))
  
  # Graph 1: Actual output Vs PWL Output
  png("Actual Output Vs PWL Output.png", width = 900, height = 600)
  plot(cbind(ActualOutput, pwlOutput[,2]), ylim=c(minY, maxY), xlim=c(min(ActualOutput), max(ActualOutput)), pch = 16, axes = FALSE, xlab="", ylab="", col="black", main="Actual Output Vs PWL Output")
  axis(2, ylim=c(minY, maxY), col="black", las=1)
  mtext("PWL Output", side=2, line= 2.5)
  axis(1, pretty(range(ActualOutput), 10))
  mtext("Actual Output", side=1, line=2.5)
  legend("topleft", legend=c(paste0("PWL MSE = ", round(pwlMSE, 7)), paste0("PWL R2 = ", round(pwlR2,4))), pch = 16, col="black")
  
  dev.off()
  
  # Graph 2: Actual output Vs ANN Output
  png("Actual Output Vs ANN Output.png", width = 900, height = 600)
  plot(cbind(ActualOutput, ANNOutput), ylim=c(minY, maxY), xlim=c(min(ActualOutput), max(ActualOutput)), pch = 4, axes = FALSE, xlab="", ylab="", col="red", main="Actual Output Vs ANN Output")
  axis(2, ylim=c(minY, maxY), col="black", las=1)
  mtext("ANN Output", side=2, line= 2.5)
  axis(1, pretty(range(ActualOutput), 10))
  mtext("Actual Output", side=1, line=2.5)
  legend("topleft", legend=c(paste0("ANN MSE = ", round(annMSE, 7)), paste0("ANN R2 = ", round(annR2,4))), pch = 4, col="red", text.col="red")
  
  dev.off()
  
  # Graph 3: Actual output Vs PWL and ANN Output
  png("Actual Output Vs Predicted Output.png", width = 900, height = 600)
  plot(cbind(ActualOutput, pwlOutput[,2]), ylim=c(minY, maxY), xlim=c(min(ActualOutput), max(ActualOutput)), pch = 16, axes = FALSE, xlab="", ylab="", col="black", main="Actual Output Vs Predicted Output")
  axis(2, ylim=c(minY, maxY), col="black", las=1)
  mtext("PWL Output", side=2, line= 2.5)
  par(new=T)
  plot(cbind(ActualOutput, ANNOutput),ylim=c(minY, maxY), xlim=c(min(ActualOutput), max(ActualOutput)), pch = 4, axes = FALSE, xlab="", ylab="", col="red")
  axis(4, ylim=c(minY, maxY), col="red", las=1)
  mtext("ANN Output", side=4, col="red", line= 2.5)
  axis(1, pretty(range(ActualOutput), 10))
  mtext("Actual Output", side=1, line=2.5)
  legend("topleft", legend=c("PWL Output", paste0("PWL MSE = ", round(pwlMSE, 7)), paste0("PWL R2 = ", round(pwlR2,4)), "ANN Output", paste0("ANN MSE = ", round(annMSE, 7)), paste0("ANN R2 = ", round(annR2,4))), pch = c(16, 16, 16, 4, 4, 4), col=c("black", "black", "black", "red", "red", "red"), text.col=c("black", "black", "black", "red", "red", "red"))
  
  dev.off()
}