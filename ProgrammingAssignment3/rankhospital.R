rankhospital <- function(state, outcome, num = "best") {
  
  #Read csv file 
  dataset <<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Validate state name
  if (!state %in% dataset$State)
  { 
    stop("Invalid state name!")
  }
  
  #Extract state data
  matchSet <<- subset(dataset,State==state,na.rm=TRUE)
  
  if (num == "best") num <-as.numeric(1)
  
  
  if (outcome == "heart attack")
  {
    nummatchSet <- as.numeric(matchSet[,11])
    if (num == "worst") num <-sum(!is.na(nummatchSet))
    index<-order(nummatchSet,matchSet$Hospital.Name)[num]
    matchSet[index,]$Hospital.Name
  }
  else if (outcome == "heart failure")
  {
    nummatchSet <<- as.numeric(matchSet[,17])
    if (num == "worst") num <-sum(!is.na(nummatchSet))
    index<-order(nummatchSet,matchSet$Hospital.Name)[num]
    matchSet[index,]$Hospital.Name
  }
  else if (outcome == "pneumonia")
  {
    nummatchSet <<- as.numeric(matchSet[,23])
    if (num == "worst") num <-sum(!is.na(nummatchSet))
    index<-order(nummatchSet,matchSet$Hospital.Name)[num]
    matchSet[index,]$Hospital.Name
  }
  else
  { 
    stop("Invalid outcome name!")
  }
}