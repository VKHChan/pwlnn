# function that checks if the given data is a "mid" section,
# i.e. if the data set crosses the x=0
# This is used in getequataion.nn and calculateSSRmatrix.
# If the data is a mid section, then it has to pass through the origin, i.e. lm(y~x+0)

ismidsection <- function(data){
  
  data <- as.data.frame(data)
  #if the first data point is greater than zero, then it's not a mid section
  if(data[1,1] > 0){
    check <- FALSE
  }else if(data[nrow(data),1] < 0){
  # if the last data is smaller than 0, then it's not a mid section either
    check <- FALSE
  }else{
    check <- TRUE
  }
  
  check
    
}