rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


  ## Check that state and outcome are valid
  states <- sort(unique(outcome_data[,7]))
  if (!is.element(state, states))
    stop("invalid state")
  if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia")))
    stop("invalid outcome")


  ## For each state, find the hospital of the given rank
  col = 0
  if (outcome == "heart attack")
    col = 11
  else if (outcome == "heart failure")
    col = 17
  else if (outcome == "pneumonia")
    col = 23

  outcome_data[,col] <- as.numeric(outcome_data[,col])

  state_ranks <- data.frame()

  for (state in states) {
    state_rank <- data.frame(state, rankstate(outcome_data, state, col, num))
    state_ranks <- rbind(state_ranks, state_rank)
  }

  ## Return a data frame with the hospital
  ## names and the (abbreviated) state name
  names(state_ranks) <- c("hospital","state")
  state_ranks
}

rankstate <- function(data, state, col, num) {
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
