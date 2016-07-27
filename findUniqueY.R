# Function that finds the unique Y Equations given the sets of Y Equations of the data set
# The unique equations defines the "Y classes"

findUniqueY <- function(YEquations){

  YEquationUnique <- unique(YEquations)
  class <- matrix(1:nrow(YEquationUnique), ncol=1)
  colnames(class) <- c("Y Class")
  YEquationUnique <- cbind(YEquationUnique, class)

  #write.xlsx(YEquationUnique, "Y Class.xlsx")
  
  YEquationUnique
}