source("getAirPollutionFilename.R")

complete <- function(directory, id = 1:332) {
  resultMatrix <- matrix(ncol = 2, nrow = 0)
  for (filename in id) {
    table <- read.csv(getAirPollutionFilename(directory, filename), header = TRUE)
    resultMatrix <- rbind(resultMatrix,c(filename, dim(subset(table, !is.na(ID) & !is.na(sulfate) & !is.na(nitrate) & !is.na(Date)))[1]))
  }
  
  resultMatrix[sort(order(resultMatrix)[id])]
  
  dataFrame <- as.data.frame(na.omit(resultMatrix))
  names(dataFrame) <- c("id", "nobs")
  dataFrame
}