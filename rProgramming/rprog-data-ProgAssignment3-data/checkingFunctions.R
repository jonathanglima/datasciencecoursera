checkStateIsValid <- function(table, state) {
  if (!any(table$State == state, na.rm = TRUE)) {
    stop("invalid state")
  }
}

checkOutcomeIsValid <- function(outcome) {
  if (!any(c("heart attack", "heart failure", "pneumonia") == outcome, na.rm = TRUE)) {
    stop("invalid outcome")
  }
}