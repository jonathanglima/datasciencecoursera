source("checkingFunctions.R")
source("readOutcomeData.R")

best <- function(state, outcome) {
  ## Read outcome data
  table <- readOutcomeData()
  ## Check that state and outcome are valid
  checkStateIsValid(table, state)
  checkOutcomeIsValid(outcome)
  ## Return hospital name in that state with lowest 30-day death
  table <- table[which(table$State == state),]
  
  if (outcome == "heart attack") {
    table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    tmp <- min(subset(table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, !is.na(table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
    table <- table[which(table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == tmp),]
  } else if (outcome == "heart failure") {
    table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    tmp <- min(subset(table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, !is.na(table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
    table <- table[which(table$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == tmp),]
  } else if (outcome == "pneumonia") {
    table$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(table$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    tmp <- min(subset(table$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, !is.na(table$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
    table <- table[which(table$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == tmp),]
  }
  
  ## rate
  return(table[order(table$Hospital.Name),]$Hospital.Name)
}