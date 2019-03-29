setwd("~/Acads/SS 2018-2019/Stat 197 DS/specdata")
library(data.table)

### PART 1 ### ----

pollutantmean <- function(directory, pollutant, id= 1:332){
  
  pollutants = NULL #creates empty vector
  
  filenames = list.files(directory) #lists files in the directory
  
  for(i in id){
    
    ##concatenates the directory and filenames
    ##directory = C:/Users/ASUS/Documents/Acads/SS 2018-2019/Stat 197 DS/specdata 
    ##filenames = vector("001.csv", "002.csv", ...)
    ##filepath= "C:/Users/ASUS/Documents/Acads/SS 2018-2019/Stat 197 DS/specdata/001.csv" 
    filepath=paste(directory, "/", filenames[i], sep="")
    
    data = read.csv(filepath, header = TRUE) ##read in each file and store it in data vector
    
    ##concatenate the vectors from each file of the pollutant column to pollutants vector
    pollutants = c(pollutants, data[,pollutant])
    
  }
  #NA values are removed and calculates the mean of the pollutants vector
  pollutants_mean = mean(pollutants, na.rm=TRUE)
  
  return(pollutants_mean)
}

#example

pollutantmean("~/Acads/SS 2018-2019/Stat 197 DS/specdata", "sulfate", 1:10)
pollutantmean("~/Acads/SS 2018-2019/Stat 197 DS/specdata", "nitrate", 70:72)

#quiz

pollutantmean("~/Acads/SS 2018-2019/Stat 197 DS/specdata", "sulfate", 1:10)
pollutantmean("~/Acads/SS 2018-2019/Stat 197 DS/specdata", "nitrate", 70:72)
pollutantmean("~/Acads/SS 2018-2019/Stat 197 DS/specdata", "sulfate", 34)
pollutantmean("~/Acads/SS 2018-2019/Stat 197 DS/specdata", "nitrate")


### PART 2 ### ----

corr <- function(directory, threshold=0){
  
  correlations <- NULL #empty correlations vector
  
  filenames = list.files(directory) #lists filenames
  
  for(i in 1:332){
    
    ##concatenates the directory and filenames
    ##directory = C:/Users/ASUS/Documents/Acads/SS 2018-2019/Stat 197 DS/specdata 
    ##filenames = vector("001.csv", "002.csv", ...)
    ##filepath= "C:/Users/ASUS/Documents/Acads/SS 2018-2019/Stat 197 DS/specdata/001.csv" 
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    data = read.csv(filepath, header = TRUE) #reads each file and stores it in the data vector
    
    completeCases = data[complete.cases(data),] #calculates the number of complete cases
    
    count = nrow(completeCases) #counts the number of complete cases
    
    if( count >= threshold ) {
      correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  return(correlations)
}

#example

cr <- corr("~/Acads/SS 2018-2019/Stat 197 DS/specdata")
cr

#quiz

cr <- corr("~/Acads/SS 2018-2019/Stat 197 DS/specdata")
cr <- sort(cr)
set.seed(868)
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("~/Acads/SS 2018-2019/Stat 197 DS/specdata", 129)
cr <- sort(cr)
n <- length(cr)
set.seed(197)
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("~/Acads/SS 2018-2019/Stat 197 DS/specdata", 2000)
n <- length(cr)
cr <- corr("~/Acads/SS 2018-2019/Stat 197 DS/specdata", 1000)
cr <- sort(cr)
print(c(n, round(cr, 4)))

### PART 3 ### ----

complete <- function(directory, id= 1:332){
  
  ids = NULL #empty vector

  nobss = NULL #empty vector
  
  filenames = list.files(directory) #lists files
  
  for(i in id){
    
    ###concatenates the directory and filenames
    ##directory = C:/Users/ASUS/Documents/Acads/SS 2018-2019/Stat 197 DS/specdata 
    ##filenames = vector("001.csv", "002.csv", ...)
    ##filepath= "C:/Users/ASUS/Documents/Acads/SS 2018-2019/Stat 197 DS/specdata/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    data = read.csv(filepath, TRUE) #reads each file
    
    completeCases = data[complete.cases(data), ] #subsets with complete observations
    
    ids =  c(ids, i) 
    
    nobss = c(nobss, nrow(completeCases) )
    
  }
  
  data.frame(id=ids, nobs=nobss)
}

#example

complete("~/Acads/SS 2018-2019/Stat 197 DS/specdata",c(1:15))
complete("~/Acads/SS 2018-2019/Stat 197 DS/specdata",c(1,4,8,5,12))

#quiz
cc <- complete("~/Acads/SS 2018-2019/Stat 197 DS/specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("~/Acads/SS 2018-2019/Stat 197 DS/specdata", 54)
print(cc$nobs)
set.seed(42)
cc <- complete("~/Acads/SS 2018-2019/Stat 197 DS/specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])