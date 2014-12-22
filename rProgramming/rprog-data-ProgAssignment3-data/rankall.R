source("readOutcomeData.R")
source("checkingFunctions.R")
source("rankhospital.R")

rankall <- function(outcome, num = "best") {
  table <- readOutcomeData()
  uniqueStates <- unique(table[order(table$State), "State"])
  hospitalList <- sapply(uniqueStates, rankhospital, outcome = outcome, num = num)
  data.frame(hospital = hospitalList, state = uniqueStates)
}
