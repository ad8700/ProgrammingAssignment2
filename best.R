library(tidyverse)

best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("./outcome-of-care-measures.csv", header=TRUE)
  ## Filter the dataset to only the columns we are interested in
  outcomes <- as.data.frame(cbind(outcomes[,2], #hospital.name
                                  outcomes[,7], #state
                                  outcomes[,11], #heart attack
                                  outcomes[,17], #heart failure
                                  outcomes[,23])) #pneumonia
  ## rename the columns
  colnames(outcomes) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  State_Check <- c(outcomes$state)
  Outcome_Check <- c("heart attack", "heart failure", "pneumonia")
  
  if(!state %in% State_Check) stop("Invalid state provided")
  state
  
  if(!outcome %in% Outcome_Check) stop("Invalid outcome provided")
  outcome
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  #Filter to only hospitals in the selected state
  hOutcomes <- outcomes[(outcomes[,"state"]==state),]
  
  #convert the outcome rates to numeric instead of char
  hOutcomes[,outcome] <- as.numeric(hOutcomes[,outcome])
  
  #remove NA or blank values
  hOutcomes <- hOutcomes[!is.na(hOutcomes[,outcome]),]
  
  #Order the data by the score and in case of a tie by hospital name
  hNames <- hOutcomes[hOutcomes[,outcome]==min(hOutcomes[,outcome]),1]
  sort(hNames)[1]
}