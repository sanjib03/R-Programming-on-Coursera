setwd("C:/Users/Sanjib/Documents/Coursera/Assignment_3")

rankall <- function(outcome, num = "best"){
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% validOutcome){ stop("Invalid outcome")}
  
  uniqueState <- sort(unique(data[,7]))
  #if(!state %in% validState) {stop("Invalid state")}
  
  fullColName <-  c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome, validOutcome)]
  
  hospital<- data.frame()
  
  for (i in seq_along(uniqueState)){
    
    data.state <- data[data$State==uniqueState[i],]
    
    sorted.data.state <- data.state[order( as.numeric(data.state[[colName]]), data.state["Hospital.Name"],na.last = NA),]
   
     rank=num  #assigning value of num to a local variable in each iteration
    if (num =="best") rank = 1
    if (num =="worst") rank= nrow(sorted.data.state)
    
    hospital <- rbind(hospital, data.frame(state=uniqueState[i],hospital=sorted.data.state[rank, "Hospital.Name",]) )
    }
  
  hospital
}

##Testing
# head(rankall("heart attack", 20), 2)
# r <- rankall("heart failure", 10)
# as.character(subset(r, state == "NV")$hospital)
