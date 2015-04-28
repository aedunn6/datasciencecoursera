rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if (!is.element(state, unique(outcome_data[,7])))
    stop("invalid state")
  if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia")))
    stop("invalid outcome")

  ## Return hospital name in that state with
  ## the given rank of 30-day death rate
  col = 0
  if (outcome == "heart attack")
    col = 11
  else if (outcome == "heart failure")
    col = 17
  else if (outcome == "pneumonia")
    col = 23

  outcome_data[,col] <- as.numeric(outcome_data[,col])
  mort <- na.omit(outcome_data[, c(2, 7, col)])
  state_mort <- mort[mort[2] == state,]
  ranked <- state_mort[order(state_mort[,3], state_mort[,1]),]
  
  last <- length(ranked[,3])
  if (num == "best")
    num = 1
  else if (num == "worst")
    num = last
  else if (num > last)
    return(NA)

  ranked[num,][,1]
}
