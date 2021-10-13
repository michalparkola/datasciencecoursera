rankhospital <- function(hospitals, state, outcome, num = "best") {

  isValidOutcome <- outcome %in% c("pneumonia", "heart attack", "heart failure")
  if(!isValidOutcome) {
    stop("invalid outcome")
  }
  
  isValidState <- state %in% unique(hospitals$State)
  if(!isValidState) {
    stop("invalid state")
  }
  
  hospitals <- subset (hospitals, hospitals$State == state)
  
  if(outcome == "heart attack") {
    ord <- order(as.numeric(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                 hospitals$Hospital.Name)
    hospitals <- hospitals[ord,]
    hospitals <- subset (hospitals,
                         hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
  }
  else if (outcome == "heart failure") {
    ord <- order(as.numeric(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                 hospitals$Hospital.Name)
    hospitals <- hospitals[ord,]
    hospitals <- subset (hospitals,
                         hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
  }
  else if (outcome == "pneumonia") {
    ord <- order(as.numeric(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                 hospitals$Hospital.Name)
    hospitals <- hospitals[ord,]
    hospitals <- subset (hospitals,
                         hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available")
  }
  
  if(num == "best")
    return(hospitals$Hospital.Name[1])
  else if(num == "worst")
    return(tail(hospitals$Hospital.Name, 1)) 
  else if(num > length(hospitals))
    return(NA)
  else return(hospitals$Hospital.Name[num])
}

rankall <- function(outcome, num = "best") {
  filename = "outcome-of-care-measures.csv"
  hospitals <- read.csv(filename, stringsAsFactors=FALSE)
  
  isValidOutcome <- outcome %in% c("pneumonia", "heart attack", "heart failure")
  if(!isValidOutcome) {
    stop("invalid outcome")
  }
  
  state <- sort(unique(hospitals$State))
  hospital <- lapply(state, function(st) rankhospital(hospitals, st, outcome, num))
  rankedbystate <- as.data.frame(cbind(hospital, state), row.names=2)
}