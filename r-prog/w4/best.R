best <- function(state, outcome) {
  filename = "outcome-of-care-measures.csv"
  outcomes <- read.csv(filename, stringsAsFactors=FALSE)
  
  isValidOutcome <- outcome %in% c("pneumonia", "heart attack", "heart failure")
  if(!isValidOutcome) {
    stop("invalid outcome")
  }
  
  isValidState <- state %in% unique(outcomes$State)
  if(!isValidState) {
    stop("invalid state")
  }

  outcomesInState <- subset (outcomes, outcomes$State == state)
  
  # Alphabetize by Hospital Name to correctly handle ties
  ord <- order(outcomesInState$Hospital.Name)
  outcomesInState <- outcomesInState[ord,]
  
  if(outcome == "heart attack")
    i <- which.min(as.numeric(outcomesInState[,11]))
  else if (outcome == "heart failure")
    i <- which.min(as.numeric(outcomesInState[,17]))
  else if (outcome == "pneumonia")
    i <- which.min(as.numeric(outcomesInState[,23]))
    
  outcomesInState$Hospital.Name[i]
}
