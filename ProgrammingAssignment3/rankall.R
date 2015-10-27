rankall <- function(outcome, num = "best") {
    
  #Read csv file 
  dataset <<- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  uniqueState <- unique(dataset$State)
  stateIndex <- order(uniqueState)
  orderState <- uniqueState[stateIndex]
  
  if (num == "best") num <-as.numeric(1)
  for (i in length(orderState):1)
  {
  state<-orderState[i]

  #Extract state data
  matchSet <- subset(dataset,State==state,na.rm=TRUE)
  
  if (outcome == "heart attack")
  {
    nummatchSet <- as.numeric(matchSet[,11])
    if (num == "worst") num <-sum(!is.na(nummatchSet))
    index<-order(nummatchSet,matchSet$Hospital.Name)[num]
    hosName <- matchSet[index,]$Hospital.Name
  }
  else if (outcome == "heart failure")
  {
    nummatchSet <- as.numeric(matchSet[,17])
    if (num == "worst") num <-sum(!is.na(nummatchSet))
    index<-order(nummatchSet,matchSet$Hospital.Name)[num]
    hosName <- matchSet[index,]$Hospital.Name
  }
  else if (outcome == "pneumonia")
  {
    nummatchSet <- as.numeric(matchSet[,23])
    if (num == "worst") num <-sum(!is.na(nummatchSet))
    index<-order(nummatchSet,matchSet$Hospital.Name)[num]
    hosName <- matchSet[index,]$Hospital.Name
  }
  else
  { 
    stop("Invalid outcome name!")
  }
  
  if(!exists("myFrame")){
    myFrame <- data.frame(hosName,state)
  }
  else{
    myFrame <- rbind(myFrame,data.frame(hosName,state))
  }
  }
  colnames(myFrame) <- c("hospital","state")
  return(myFrame)
  rm(myFrame)
}