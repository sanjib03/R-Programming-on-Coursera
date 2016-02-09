
getwd()
setwd("C:/Users/Sanjib/Documents/Coursera/Assignment_1")

corr <- function(directory, threshold=0) {
  
  fileNames<- list.files(directory,pattern = "*.csv",full.names = TRUE)
  dta<- data.frame()
  correlationVector=c()
  
  for (i in 1:332) {
    dta<-read.csv(fileNames[i], header = TRUE)
    if (sum(complete.cases(dta)) > threshold){
      
      data<- complete.cases(dta)
      #dt<- as.matrix(data)
      correlationVector<- c(correlationVector, cor(dta[data,2],dta[data,3]))
      #correlationVector<- c(correlationVector, cor(data[,2],data[,3]))
    }
  }
  correlationVector
}


cr <- corr("specdata", 400)
head(cr); summary(cr)

#######
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
##########

cr <- corr("specdata", 129)                
cr <- sort(cr)
n <- length(cr)                
set.seed(197) 
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
####

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))