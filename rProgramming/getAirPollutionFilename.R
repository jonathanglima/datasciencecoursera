getAirPollutionFilename <- function(directory, filename, addExtension = TRUE) {
  realId <- if (nchar(filename) == 1) {
    paste("00", toString(filename), sep = '')
  } else if (nchar(filename) == 2) {
    paste("0", toString(filename), sep = '')
  } else {
    toString(filename)
  }
  
  if (addExtension) {
    paste(directory, "/", realId, ".csv", sep = '')
  } else {
    paste(directory, "/", realId, sep = '')
  }
}