source("complete.R")

corr <- function(directory, threshold = 0) {  
  total <- numeric()
  
  for(filename in list.files(directory)) {
    table <- read.csv(getAirPollutionFilename(directory, filename, addExtension = FALSE), header = TRUE)
    completeCases <- nrow(table[complete.cases(table),])
    
    if (completeCases > threshold) {
      total <- c(total, cor(table$nitrate, table$sulfate, use = "complete.obs"))
    }
  }
  
  as.numeric(total)
}