#Part 2
best <- function(state, outcome) {
  
  ## all possible argument strings for outcome
  outcomes = c("heart attack", "heart failure", "pneumonia")
  
  ## to check if the argument for outcome entered by the user matches the choices above
  ## %in% returns a logical vector indicating if there is a match or not, prints "invalid outcome" if entered argument does not match
  if( outcome %in% outcomes == FALSE ) {
    stop("invalid outcome")
  }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Get the columns below from 'data' and place it in 'data' with new names ("name", "state", "heart attack", etc)
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ## Get a vector from the data of all possible argument strings for state
  states <- data[, 2]
  states <- unique(states)
  ## to check if the argument for state entered by the user matches the choices in the vector "states"
  if( state %in% states == FALSE ) {
    stop("invalid state")
  }
  
  ## Get only the rows with our state value	
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  vals <- data[, outcome]
  
  ## RowNum = the index of the minimum value 
  rowNum <- which.min(vals)
  
  ## Return hospital name in that state with lowest 30-day death rate
  data[rowNum, ]$name
}

best("TX", "heart attack")
best("TX", "heart failure")
best("TX", "pneumonia")
