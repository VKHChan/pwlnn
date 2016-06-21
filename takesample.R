# function that takes a sample of the given dataset
# sample data size should be less than 600 entries

takesample <- function(data){
  
  i <- round(nrow(data)/600)
  sample <- data[seq(1, nrow(data), i),]
  
  sample
}