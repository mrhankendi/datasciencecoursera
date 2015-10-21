#Programming Assignment 3
best <- function(state, outcome) {
  
  #Read csv file 
  dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Validate state name
  if (!state %in% dataset$State)
  { 
    stop("Invalid state name!")
  }
  
  #Extract state data
  matchSet <<- subset(dataset,State==state,na.rm=TRUE)
  
  #Validate outcome
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  
  if (outcome == "heart attack")
  {
    matchSet[which.min(matchSet[,11]), ]$Hospital.Name
  }
  else if (outcome == "heart failure")
  {
    matchSet[which.min(matchSet[,17]), ]$Hospital.Name
  }
  else if (outcome == "pneumonia")
  {
    matchSet[which.min(matchSet[,23]), ]$Hospital.Name
  }
  else
  { 
    stop("Invalid outcome name!")
  }

}
