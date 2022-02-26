library(tidyverse)

rankhospital <- function(state, outcome, num = 'best') {
  #read in the outcome data
  outcomes <- read.csv("./outcome-of-care-measures.csv", header=TRUE)
  
  #limit to the columns that we are interested in
  df <- as.data.frame(cbind(outcomes[,2],
                            outcomes[,7],
                            outcomes[,11],
                            outcomes[,17],
                            outcomes[,23]),
                      stringsAsFactors=FALSE)
  #rename the coulmns to something more helpful
  colnames(df) <- c("hospital","state","heart attack","heart failure","pneumonia")
  
  # Check that state and outcome are valid
  State_Check <- c(df$state)
  Outcome_Check <- c("heart attack", "heart failure", "pneumonia")
  
  if(!state %in% State_Check) stop("Invalid state provided")
  state
  
  if(!outcome %in% Outcome_Check) stop("Invalid outcome provided")
  outcome
  
  #Filter to only hospitals in the selected state
  hOutcomes <- df[(df[,"state"]==state),]
  
  #convert the outcome rates to numeric instead of char
  hOutcomes[,outcome] <- as.numeric(hOutcomes[,outcome])
  
  #remove NA or blank values
  hOutcomes <- hOutcomes[!is.na(hOutcomes[,outcome]),]
  
  # Take the argument and convert to a rank
  if(num=="best") {
    num <- 1
  }
  
  if(num=="worst") {
    num <- nrow(hOutcomes)
  }
  
  hOutcomes <- hOutcomes[order(hOutcomes[,outcome], hOutcomes[,"hospital"]),]
  
  #get the name of the hospital
  hOutcomes[num, 1]
  
}