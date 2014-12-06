source("getAirPollutionFilename.R")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- vector('character')
  for (filename in id) {
      files[filename] <- getAirPollutionFilename(directory, filename)
  }

  pollutantTables <- do.call(rbind, lapply(na.omit(files), read.csv, header = TRUE))
  mean(pollutantTables[,pollutant], na.rm = TRUE)
}
