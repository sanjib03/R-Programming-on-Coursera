
getwd()
setwd("C:/Users/Sanjib/Documents/Coursera/Assignment_3")

best <- function(state, outcome){
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available") 
  
  #check the validity of state and outcome
  validOutcome <- c("heart attack","heart failure", "pneumonia")
  if(!outcome %in% validOutcome) {
    stop("Invalid outcome") }
  
  validState <- unique(data[,7])
  if(!state %in% validState) {stop("invalid state")}
  
  #Convert outcome into full column name
  fullColName <-  c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- data[data$State==state,]
  indx <- which.min(as.double(data.state[,colName]))
  data.state[indx,"Hospital.Name"]
  
}
##Testing
#best("TX", "heart attack")


