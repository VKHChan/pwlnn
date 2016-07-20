# function that takes a sample of the given dataset
# sample data size should be less than 300 entries

takesample <- function(data, samplesize){
  
  i <- round(nrow(data)/samplesize)
  sample <- data[seq(1, nrow(data), i),]
  
  sample
}