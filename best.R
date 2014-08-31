best <- function(state, outcome) {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state is valid
  if (state %in% outcomedata$State){
    ## Check that outcome is valid
    if (outcome %in% c("heart attack", "heart failure" , "pneumonia")){
      selection <- with(outcomedata, outcomedata[State == state,])
      ## Based on the outcome get the subset of the selection data frame
      if (outcome == "heart attack"){
        selection <- selection[c('State','Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')]
        selection <- transform(selection,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      } else if (outcome == "heart failure"){
        selection <- selection[c('State','Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')]
        selection <- transform(selection,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      } else if (outcome == "pneumonia"){
        selection <- selection[c('State','Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')]
        selection <- transform(selection,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      } 
      selection <- selection[complete.cases(selection),]
      selection <- selection[selection[,3] == min(selection[,3],na.rm = TRUE),]
      selection <- max(selection$Hospital.Name)
    } else{
      stop("Invalid outcome")
    }
  } else{
      stop("Invalid state")
   }
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  options(warn=-1)
  selection
  
}