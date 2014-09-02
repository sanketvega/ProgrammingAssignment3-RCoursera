rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  numNumeric <- NULL
  rankselection <- data.frame()
  
  uniquestate <- unique(outcomedata$State)
  for(currentstate in uniquestate){
    ## Check that state is valid
    if (currentstate %in% currentstate){
      ## Check that outcome is valid
      if (outcome %in% c("heart attack", "heart failure" , "pneumonia")){
        selection <- with(outcomedata, outcomedata[State == currentstate,])
        if (outcome == "heart attack"){
          selection <- selection[c('State','Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')]
          selection <- transform(selection,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
          ## selection <- selection[with(selection,order(Hospital.Name)),]
          selection <- transform(selection, Rank = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, FUN = function(x) rank(x, ties.method = "first")))        
          
        } else if (outcome == "heart failure"){
          selection <- selection[c('State','Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')]
          selection <- transform(selection,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
          ## selection <- selection[with(selection,order(Hospital.Name)),]
          selection <- transform(selection, Rank = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, FUN = function(x) rank(x, ties.method = "first")))
          
        } else if (outcome == "pneumonia"){
          selection <- selection[c('State','Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')]
          selection <- transform(selection,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
          ## selection <- selection[with(selection,order(Hospital.Name)),]
          selection <- transform(selection, Rank = ave(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, FUN = function(x) rank(x, ties.method = "first")))
        } 
    
        ## Convert rank argument to numeric
        if (num == "best"){
          numNumeric <- 1
        } else if (num == "worst"){
          numNumeric <- max(selection$Rank)
        } else{
          numNumeric <- as.numeric(num)
        }
        ## Sort the selection based on Rank
        ## selection <- selection[with(selection,order(Rank)),]
        ## Select the value from selection where rank is in the provided value
        selection <- selection[selection[,4] == numNumeric,]
        rankselection <- rbind(rankselection,selection)
      } else{
        stop("Invalid outcome")
      }
    } else{
      stop("Invalid state")
    }

    
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  options(warn=-1)
  ## rankselection <- rankselection[with(rankselection,order(State)),]
  rankselection <- rankselection[c('Hospital.Name','State')]
  names(rankselection) <- c("hospital","state")
  rankselection
}
