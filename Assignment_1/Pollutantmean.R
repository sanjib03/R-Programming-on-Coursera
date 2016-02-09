
getwd()
setwd("C:/Users/Sanjib/Documents/Coursera/Assignment_1")

pollutantmean <- function(directory, pollutant, id = 1:332){
  
  fileNames<- list.files(directory,pattern = "*.csv",full.names = TRUE)
  dta<- data.frame()
  for(i in id) {
  dta<-rbind(dta,read.csv(fileNames[i]))
  
  }
  if (pollutant=='sulfate') {
    mean(dta$sulfate, na.rm = TRUE)
  }
  else if (pollutant=='nitrate')
  {
    mean(dta$nitrate, na.rm = TRUE)
  }
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
