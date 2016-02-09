
getwd()
setwd("C:/Users/Sanjib/Documents/Coursera/Assignment_1")

complete <- function(directory, id = 1:332) {
  fileNames<- list.files(directory,pattern = "*.csv",full.names = TRUE)
  dta<- data.frame()
  completeCases<- data.frame()
  for (i in id) {
    dta<-read.csv(fileNames[i], header = TRUE)
    Obs<-sum(complete.cases(dta))
    completeCases<- rbind(completeCases, data.frame(i,Obs))
  }
  completeCases
}

######### Testing
complete("specdata", c(2, 4, 8, 10, 12))
 cc<- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$Obs)
#######
cc <- complete("specdata", 54)
print(cc$Obs)
#########
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "Obs"])

