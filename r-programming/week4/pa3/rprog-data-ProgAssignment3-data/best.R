best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if (!is.element(state, unique(outcome_data[,7])))
    stop("invalid state")
  if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia")))
    stop("invalid outcome")

  ## Return hospital name in that state with lowest 30-day death rate
  # heart attack mortality percentages: column 11
  # heart failure mortality percentages: column 17
  # pneumonia mortality percentages: column 23
  col = 0
  if (outcome == "heart attack")
    col = 11
  else if (outcome == "heart failure")
    col = 17
  else if (outcome == "pneumonia")
    col = 23

  mort <- na.omit(outcome_data[, c(2, col)])
  best_hosps <- mort[mort[2] == min(mort[2]),][1]
  return sort(best_hosps)[1]
}
