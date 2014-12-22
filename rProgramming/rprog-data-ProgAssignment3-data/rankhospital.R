source("readOutcomeData.R")
source("checkingFunctions.R")

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  table <- readOutcomeData()
  ## Check that state and outcome are valid
  checkStateIsValid(table, state)
  checkOutcomeIsValid(outcome)
  
  field <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  table <- table[table$State == state, c("Hospital.Name", field)]
  if (nrow(table) == 0) {
    stop("invalid state")  
  }
  
  names(table)[names(table) == field] <- "Rank"
  table$Rank <- as.numeric(table$Rank)
  orderIndex <- order(table$Rank, table$Hospital.Name, na.last=NA)

  if (num == "best") {
    num <- orderIndex[1]
  } else if (num == "worst") {
    num <- orderIndex[length(orderIndex)]
  } else if (is.numeric(num)) {
    num <- orderIndex[num]
  } else {
    stop("invalid num")
  }
  
  as.character(table$Hospital.Name[num])
}
