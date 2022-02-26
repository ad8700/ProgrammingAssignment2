library(tidyverse)

rankall <- function(outcome, num="best") {
  #read in the outcome data
  outcomes <- read.csv("./outcome-of-care-measures.csv", header=TRUE)
  
  #limit to the columns that we are interested in
  df <- as.data.frame(cbind(outcomes[,2],
                            outcomes[,7],
                            outcomes[,11],
                            outcomes[,17],
                            outcomes[,23]),
                      stringsAsFactors=FALSE)
  #rename the columns to something more helpful
  colnames(df) <- c("hospital","state","heart attack","heart failure","pneumonia")
  
  # Check that the outcome is valid
  Outcome_Check <- c("heart attack", "heart failure", "pneumonia")
  num_Check <- c("best", "worst", 1:nrow(df))
  
  if(!outcome %in% Outcome_Check) stop ("Invalid outcome provided")
  outcome
  
  if(!num %in% num_Check) stop ("Must provide either best, worst or an integer")
  num
  
  #Return the hospital name for that state that is the best for that outcome
  
  #create an empty data frame to hold results
  
  hosp_rank <- data.frame()
  
  #filter by state
  for (state in sort(unique(df[,"state"]))) {
    hosp_outcome <- df[(df[,"state"] == state),]
    
    # convert outcome to a numeric from a char
    hosp_outcome[, outcome] <- as.numeric(hosp_outcome[,outcome])
    
    # remove NA values
    hosp_outcome <- hosp_outcome[!is.na(hosp_outcome[,outcome]), ]
    
    # Take the num argument and convert to a rank
    if(num=="best") {
      rnum <- 1
    }else if(num=="worst") {
      rnum <- nrow(hosp_outcome)
    }else {rnum = num}
    
    # Order by the outcome score and hospital name to break ties                              
    hosp_outcome <- hosp_outcome[order(hosp_outcome[,outcome],
                                       hosp_outcome[,"hospital"]),]
    
    hosp_name <- hosp_outcome[rnum,1]
    
    hosp_rank <- rbind(hosp_rank, 
                       data.frame(hospital=hosp_name,
                                  state=state))
  }
  #return the ranking data frame here
  hosp_rank
}